# ============================================================
#  func_data.R
#  Dataåtkomst för branschstatistik.
#
#  Källor (databasen "oppna_data"):
#   - mikro_db.syss_branscher  -- själva statistiken. Kolumner (bekräftat
#     mot verklig data): ar, alder, bakgrund, regionkod_ast, regionkod_bo,
#     region_ast, region_bo, kon, branschkod, bransch, matt, antal.
#     Population = sysselsatta med arbetsställe i Dalarnas kommuner,
#     länet (regionkod_ast == "20") eller riket (regionkod_ast == "00").
#     regionkod_bo (bosättningskommun) finns på samma rad men används
#     INTE här -- sparas för en framtida pendlings-/rörlighetsflik.
#   - nycklar.sni_huvudgrupper -- SNI-branschkoder (2-siffrigt) med fyra
#     färdiga branschindelningar som egna kolumner (bransch_20, bransch_37,
#     bransch_52kat, bransch_61kat) samt en grövre gruppering
#     (grupp_kod/grupp_benamning). Motsvarar den uppladdade CSV-filen.
#
#  VIKTIGT: kon/alder/bakgrund saknar en "totalt"-rad -- varje rad är
#  redan nedbruten på cellnivå. En totalsumma fås genom att INTE gruppera
#  på dessa kolumner (dvs. summan sker automatiskt över dem), inte genom
#  att filtrera på ett sentinelvärde.
#
#  geo_niva härleds ur kommun_kod (regionkod_ast): "00" = riket,
#  "20" = länet (Dalarna), övriga = kommun.
# ============================================================

# ---- Branschkoder (SNI 2-siffrigt) + färdiga indelningar -----------------

# Namn -> kolumnnamn i nycklar.sni_huvudgrupper, för indelningsväljaren
# i UI:t. Inga separata dim_indelning/brygga-tabeller behövs för dessa
# fyra -- varje rad i tabellen har redan sin tillhörighet i alla fyra
# som egna kolumner. (Egna/skräddarsydda indelningar, om ni bygger det
# senare, behöver en flexibel bryggtabell precis som vi skissade
# tidigare -- det är inte samma sak som dessa fyra fördefinierade.)
INDELNING_KOLUMNER <- c(
  "Bransch (37 grupper)"    = "bransch_37",
  "Bransch (52 kategorier)" = "bransch_52kat",
  "Bransch (61 kategorier)" = "bransch_61kat",
  "Grupp (gr\u00f6vre, ~15 grupper)" = "grupp_benamning",
  "Bransch (20 grupper) \u2014 ofullst\u00e4ndig i k\u00e4lldata" = "bransch_20"
)

.dim_bransch_cache <- new.env(parent = emptyenv())

# Rensning: råtabell (svenska/CSV-kolumnnamn) -> appens interna namn,
# utan svenska tecken i kolumnnamnen. huvudgrupp nollfylls till 2 siffror
# (koder under 10 saknar inledande nolla i källan).
rensa_dim_bransch <- function(rad) {
  rad |>
    dplyr::transmute(
      branschkod      = sprintf("%02d", as.integer(huvudgrupp)),
      benamning       = benamning,
      avdelning       = avdelning,
      grupp_kod       = grupp_kod,
      grupp_benamning = grupp_benamning,
      bransch_20      = bransch_20,
      bransch_37      = bransch_37,
      bransch_52kat   = bransch_52kat,
      bransch_61kat   = bransch_61kat
    )
}

hamta_dim_bransch <- function(force = FALSE) {
  if (force || is.null(.dim_bransch_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("nycklar", "sni_huvudgrupper")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .dim_bransch_cache$df <- rensa_dim_bransch(rad)
  }
  .dim_bransch_cache$df
}

hamta_indelningar <- function() {
  tibble::tibble(namn = names(INDELNING_KOLUMNER), kolumn = unname(INDELNING_KOLUMNER))
}

# Distinkta grupper (branschnamn) inom en vald indelningskolumn. Rader
# utan gruppnamn (t.ex. "Uppgift saknas"-branschen i bransch_20, som
# saknar en motsvarighet i just den indelningen) filtreras bort.
hamta_grupper_for_indelning <- function(indelning_kolumn) {
  hamta_dim_bransch() |>
    dplyr::distinct(grupp_namn = .data[[indelning_kolumn]]) |>
    dplyr::filter(!is.na(grupp_namn), grupp_namn != "") |>
    dplyr::arrange(grupp_namn)
}

# ---- Sysselsättningsstatistik (syss_branscher) ----------------------------

.syss_cache <- new.env(parent = emptyenv())

rensa_syss_branscher <- function(rad) {
  rad |>
    dplyr::transmute(
      ar           = as.integer(ar),
      alder        = alder,
      bakgrund     = bakgrund,
      kon          = kon,
      matt         = matt,
      branschkod   = sprintf("%02d", as.integer(branschkod)),
      bransch      = bransch,
      kommun_kod   = as.character(regionkod_ast),  # tabellens population är arbetsställebaserad (dagbefolkning)
      bokommun_kod = as.character(regionkod_bo),    # bosättningskommun -- sparad för framtida pendlingsanalys, oanvänd i Översikt
      antal        = as.numeric(antal)
    )
}

hamta_syss_branscher <- function(force = FALSE) {
  if (force || is.null(.syss_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", "syss_branscher")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .syss_cache$df <- rensa_syss_branscher(rad)
  }
  .syss_cache$df
}

# Tillgängliga år, nyast först (för årväljaren i UI:t).
hamta_ar_lista <- function() {
  sort(unique(hamta_syss_branscher()$ar), decreasing = TRUE)
}

.geo_niva_for <- function(kommun_kod) {
  dplyr::case_when(
    kommun_kod == "00" ~ "riket",
    kommun_kod == "20" ~ "lan",
    TRUE                ~ "kommun"
  )
}

hamta_kommuner <- function() {
  DALARNA_KOMMUNER
}

# Returnerar sysselsatta per bransch för tre geografinivåer samtidigt
# ("vald", "lan", "riket"), enligt vald indelningskolumn och valt år.
hamta_oversiktsdata <- function(indelning_kolumn, geografi, ar_val) {
  dim_br <- hamta_dim_bransch() |>
    dplyr::select(branschkod, grupp_namn = dplyr::all_of(indelning_kolumn)) |>
    dplyr::filter(!is.na(grupp_namn), grupp_namn != "")

  bas <- hamta_syss_branscher() |>
    dplyr::filter(ar == ar_val, matt == "sysselsatta") |>
    dplyr::mutate(geo_niva = .geo_niva_for(kommun_kod)) |>
    dplyr::inner_join(dim_br, by = "branschkod")
  # OBS: kon/alder/bakgrund grupperas medvetet INTE på -- de summeras
  # automatiskt bort i group_by/summarise-stegen nedan.

  lan_riket <- bas |>
    dplyr::filter(geo_niva %in% c("lan", "riket")) |>
    dplyr::group_by(grupp_namn, geo_niva) |>
    dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop")

  vald <- dplyr::filter(bas, kommun_kod == geografi)
  vald <- vald |>
    dplyr::group_by(grupp_namn) |>
    dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(geo_niva = "vald")

  dplyr::bind_rows(vald, lan_riket) |>
    prick_smavarden(varde_kol = "antal")
}

# Totalt antal för ett givet mått ("sysselsatta"/"etablerade") i en
# geografi och år -- OBEROENDE av branschindelning (summerar alla
# branschkoder direkt, ingen koppling till dim_bransch). geografi = "00"
# fungerar som riket, eftersom regionkod_ast == "00" redan är en färdig
# riksrad i källdatan.
# Totalt antal för ett givet mått ("sysselsatta"/"etablerade") i en
# geografi och år -- OBEROENDE av branschindelning (summerar alla
# branschkoder direkt, ingen koppling till sni_huvudgrupper). geografi
# tar en riktig kommun_kod: "00" = riket, "20" = hela Dalarnas län,
# eller en enskild kommunkod -- alla tre är redan färdiga rader i
# källdatan, inget särfall behövs.
hamta_matt_totalt <- function(matt_val, geografi, ar_val) {
  hamta_syss_branscher() |>
    dplyr::filter(ar == ar_val, matt == matt_val, kommun_kod == geografi) |>
    dplyr::summarise(total = sum(antal, na.rm = TRUE)) |>
    dplyr::pull(total)
}

hamta_total_sysselsatta <- function(geografi, ar_val) {
  hamta_matt_totalt("sysselsatta", geografi, ar_val)
}

hamta_total_etablerade <- function(geografi, ar_val) {
  hamta_matt_totalt("etablerade", geografi, ar_val)
}

# --- Sekretess/döljning ---------------------------------------------------

prick_smavarden <- function(df, varde_kol, troskel = 4) {
  df |>
    dplyr::mutate(
      "{varde_kol}" := dplyr::if_else(
        .data[[varde_kol]] > 0 & .data[[varde_kol]] < troskel,
        NA_integer_,
        .data[[varde_kol]]
      )
    )
}

formatera_nyckeltal <- function(varde, troskel = 4) {
  if (is.na(varde)) return(paste0("F\u00e4rre \u00e4n ", troskel))
  format(varde, big.mark = " ", scientific = FALSE, trim = TRUE)
}
