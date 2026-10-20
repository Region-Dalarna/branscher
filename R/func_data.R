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
#     alder/regionkod_bo används INTE av appen och summeras bort redan i
#     databasen vid inläsning (se hamta_syss_branscher()); kon/bakgrund
#     läses separat vid behov (se hamta_syss_branscher_dim()) för
#     nedbrytningarna i "Sysselsatta per bransch" på Översikt.
#     En framtida pendlingsflik får göra en egen fråga på regionkod_bo.
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
      matt         = matt,
      branschkod   = sprintf("%02d", as.integer(branschkod)),
      bransch      = bransch,
      kommun_kod   = as.character(regionkod_ast),  # tabellens population är arbetsställebaserad (dagbefolkning)
      antal        = as.numeric(antal)
    )
}

hamta_syss_branscher <- function(force = FALSE) {
  if (force || is.null(.syss_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    # Ålder/kön/bakgrund/bosättningskommun används inte av appen och
    # summeras bort redan i databasen -- ger en bråkdel av raderna att
    # hämta jämfört med hela tabellen.
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", "syss_branscher")) |>
      dplyr::group_by(ar, matt, regionkod_ast, branschkod, bransch) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
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
# ("vald", "lan", "riket"), enligt vald indelningskolumn och valt år. Om
# geografi redan ÄR länet (LAN_KOD_DALARNA) utelämnas "vald" -- den vore
# bara en duplicerad kopia av "lan".
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

  if (geografi == LAN_KOD_DALARNA) {
    return(lan_riket |> prick_smavarden(varde_kol = "antal"))
  }

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
# branschkoder = NULL ger alla branscher; annars summeras bara dessa
# SNI-koder (t.ex. från hamta_branschkoder()).
hamta_matt_totalt <- function(matt_val, geografi, ar_val, branschkoder = NULL) {
  d <- hamta_syss_branscher() |>
    dplyr::filter(ar == ar_val, matt == matt_val, kommun_kod == geografi)
  if (!is.null(branschkoder)) d <- dplyr::filter(d, branschkod %in% branschkoder)
  d |>
    dplyr::summarise(total = sum(antal, na.rm = TRUE)) |>
    dplyr::pull(total)
}

hamta_total_sysselsatta <- function(geografi, ar_val, branschkoder = NULL) {
  hamta_matt_totalt("sysselsatta", geografi, ar_val, branschkoder)
}

hamta_total_etablerade <- function(geografi, ar_val, branschkoder = NULL) {
  hamta_matt_totalt("etablerade", geografi, ar_val, branschkoder)
}

# Antal sysselsatta per år, för en geografi -- hela urvalet
# (branschkoder = NULL) eller en eller flera SNI-koder (t.ex. från
# hamta_branschkoder()). Grund för tidsserien på Översikt när man
# klickar på en stapel i "Sysselsatta per bransch". dimension = "kon"
# eller "bakgrund" delar upp på den dimensionen (en rad per år och
# grupp_dim) i stället för en totalrad per år -- håller tidsserien i
# synk med Könsuppdelat/Inrikes-utrikes/Totalt-läget i stapeldiagrammet.
hamta_sysselsatta_tid <- function(geografi, branschkoder = NULL, dimension = NULL) {
  if (!is.null(dimension)) {
    d <- hamta_syss_branscher_dim(dimension) |>
      dplyr::filter(matt == "sysselsatta", kommun_kod == geografi)
    if (!is.null(branschkoder)) d <- dplyr::filter(d, branschkod %in% branschkoder)
    d |>
      dplyr::group_by(ar, grupp_dim) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(ar, grupp_dim) |>
      prick_smavarden(varde_kol = "antal")
  } else {
    d <- hamta_syss_branscher() |>
      dplyr::filter(matt == "sysselsatta", kommun_kod == geografi)
    if (!is.null(branschkoder)) d <- dplyr::filter(d, branschkod %in% branschkoder)
    d |>
      dplyr::group_by(ar) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(ar) |>
      prick_smavarden(varde_kol = "antal")
  }
}

# ---- Sysselsatta per bransch och kön/bakgrund (stapeldiagrammet på Översikt) ------
#
# Till skillnad från hamta_syss_branscher() (som medvetet summerar bort
# kön/ålder/bakgrund redan i databasen) behåller denna frågan kön resp.
# bakgrund (inrikes/utrikes född), för att kunna visa ett uppdelat
# stapeldiagram/tidsserie. De exakta textsträngarna i mikro_db.syss_branscher
# (kon- och bakgrund-kolumnerna) är inte bekräftade, så övrig kod läser
# nedbrytningsvärdena direkt ur resultatet i stället för att anta dem.

.syss_dim_cache <- new.env(parent = emptyenv())

rensa_syss_branscher_dim <- function(rad, dimension) {
  rad |>
    dplyr::transmute(
      ar         = as.integer(ar),
      matt       = matt,
      branschkod = sprintf("%02d", as.integer(branschkod)),
      kommun_kod = as.character(regionkod_ast),
      grupp_dim  = as.character(.data[[dimension]]),
      antal      = as.numeric(antal)
    )
}

# dimension = "kon" eller "bakgrund" -- vilken av mikro_db.syss_branschers
# nedbrytningskolumner som ska behållas (utöver dem som alltid grupperas
# bort, se hamta_syss_branscher()). Resultatets nedbrytningsvärde ligger
# alltid i kolumnen grupp_dim, oavsett vilken dimension som valdes, så att
# anroparen (stapeldiagrammet/tidsserien) inte behöver bry sig om vilken
# av de två det är.
hamta_syss_branscher_dim <- function(dimension = c("kon", "bakgrund"), force = FALSE) {
  dimension <- match.arg(dimension)
  if (force || is.null(.syss_dim_cache[[dimension]])) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", "syss_branscher")) |>
      dplyr::group_by(ar, matt, regionkod_ast, branschkod, .data[[dimension]]) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .syss_dim_cache[[dimension]] <- rensa_syss_branscher_dim(rad, dimension)
  }
  .syss_dim_cache[[dimension]]
}

# Antal sysselsatta per bransch (i vald indelning), uppdelat på en
# dimension (kön eller bakgrund) samt en beräknad "Totalt"-rad (källdatan
# har ingen egen totalrad för kön/bakgrund). totalt_varde avgör vilket
# värde "Totalt"-raden får i grupp_dim-kolumnen.
hamta_bransch_sysselsatta <- function(indelning_kolumn, geografi, ar_val, totalt_varde,
                                      dimension = "kon") {
  dim_br <- hamta_dim_bransch() |>
    dplyr::select(branschkod, grupp_namn = dplyr::all_of(indelning_kolumn)) |>
    dplyr::filter(!is.na(grupp_namn), grupp_namn != "")

  per_grupp <- hamta_syss_branscher_dim(dimension) |>
    dplyr::filter(ar == ar_val, matt == "sysselsatta", kommun_kod == geografi) |>
    dplyr::inner_join(dim_br, by = "branschkod") |>
    dplyr::group_by(grupp_namn, grupp_dim) |>
    dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop")

  totalt <- per_grupp |>
    dplyr::group_by(grupp_namn) |>
    dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(grupp_dim = totalt_varde)

  dplyr::bind_rows(totalt, per_grupp) |>
    prick_smavarden(varde_kol = "antal")
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
