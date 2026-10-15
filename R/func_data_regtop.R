# ============================================================
#  func_data_regtop.R
#  SCB:s Regionala trender och prognoser 2025 (RegTop2025), schema
#  regtop i databasen oppna_data. Används av fliken Prognos.
#
#  Gemensamt för tabellerna: långt format med kolumnen varde,
#  regionkod (text, "00" = riket, "20" = Dalarnas län), typ
#  ("historik"/"prognos"), enhet ("antal"/"procent"). RegTop finns
#  bara på läns- och riksnivå -- inga kommuner.
#
#  Tabellerna är små; varje tabell hämtas en gång per process (bara
#  Dalarna och riket) och sparas i minnet.
#
#  OBS: naringsgren_kod (01-24) är RegTop:s egen gruppering, inte SNI
#  2007. Koderna krockar med SNI (05 = Byggverksamhet här) -- ingen
#  join mot nycklar.sni_huvudgrupper eller syss_branscher på koden.
# ============================================================

REGTOP_REGIONER <- c("00", "20")
REGTOP_LAN      <- "20"
REGTOP_KALLA    <- "SCB, Regionala trender och prognoser 2025 (RegTop), bearbetat av Region Dalarna"

# RegTop:s grupp 24 "Uppgift saknas" -- med i totaler, inte i diagram
# per näringsgren.
REGTOP_NARINGSGREN_SAKNAS <- "24"

.regtop_cache <- new.env(parent = emptyenv())

# region = FALSE för tabeller utan regionkod (nyckeltabeller).
.regtop_hamta <- function(tabell, force = FALSE, region = TRUE) {
  if (force || is.null(.regtop_cache[[tabell]])) {
    con <- shiny_uppkoppling_las("oppna_data")
    on.exit(DBI::dbDisconnect(con))
    q <- dplyr::tbl(con, dbplyr::in_schema("regtop", tabell))
    if (region) q <- dplyr::filter(q, regionkod %in% !!REGTOP_REGIONER)
    d <- dplyr::collect(q)
    if ("ar" %in% names(d))    d$ar    <- as.integer(d$ar)
    if ("varde" %in% names(d)) d$varde <- as.numeric(d$varde)
    .regtop_cache[[tabell]] <- d
  }
  .regtop_cache[[tabell]]
}

# Förvärvsarbetande per näringsgren 2023-2040 (dagbefolkning, 16-74 år).
# 2023 = historik, 2024-2040 = prognos.
hamta_naringsgren_prognos <- function() {
  .regtop_hamta("naringsgren_prognos") |>
    dplyr::filter(enhet == "antal")
}

# Arbetskraften (förvärvsarbetande + arbetslösa, nattbefolkning, 16-74
# år) per kön och ålder 2023-2040.
hamta_arbetskraft_prognos <- function() {
  .regtop_hamta("arbetskraft_kon_alder") |>
    dplyr::filter(enhet == "antal")
}

# SCB:s fotnoter för en tabell (tabellkommentaren). NA om den saknas
# eller inte går att läsa.
hamta_regtop_fotnot <- function(tabell) {
  nyckel <- paste0("fotnot_", tabell)
  if (is.null(.regtop_cache[[nyckel]])) {
    .regtop_cache[[nyckel]] <- tryCatch({
      con <- shiny_uppkoppling_las("oppna_data")
      on.exit(DBI::dbDisconnect(con))
      DBI::dbGetQuery(con, sprintf("SELECT obj_description('regtop.%s'::regclass) AS fotnot",
                                   tabell))$fotnot[1]
    }, error = function(e) NA_character_)
  }
  .regtop_cache[[nyckel]]
}

# Prognosår (åren efter det sista historiska året), stigande.
regtop_prognosar <- function() {
  d <- hamta_naringsgren_prognos()
  sort(unique(d$ar[d$typ == "prognos"]))
}

regtop_basar <- function() {
  d <- hamta_naringsgren_prognos()
  max(d$ar[d$typ == "historik"])
}

# ---- Bearbetning --------------------------------------------------------------

# Förändring per näringsgren mellan basår och valt år, för länet och
# riket: antal_bas, antal_ar, forandring (antal), forandring_pct.
naringsgren_forandring <- function(d, basar, ar_val) {
  d |>
    dplyr::filter(ar %in% c(basar, ar_val), naringsgren_kod != REGTOP_NARINGSGREN_SAKNAS) |>
    dplyr::group_by(regionkod, region, naringsgren_kod, naringsgren) |>
    dplyr::summarise(antal_bas = sum(varde[ar == basar]),
                     antal_ar  = sum(varde[ar == ar_val]),
                     .groups = "drop") |>
    dplyr::mutate(forandring     = antal_ar - antal_bas,
                  forandring_pct = dplyr::if_else(antal_bas > 0, forandring / antal_bas, NA_real_))
}

# Total (alla grupper) för en region och ett år.
regtop_total <- function(d, region_kod, ar_val) {
  sum(d$varde[d$regionkod == region_kod & d$ar == ar_val])
}

# De n största näringsgrenarna i länet basåret (förval i utvecklingsdiagrammet).
storsta_naringsgrenar <- function(d, basar, n = 3) {
  d |>
    dplyr::filter(regionkod == REGTOP_LAN, ar == basar,
                  naringsgren_kod != REGTOP_NARINGSGREN_SAKNAS) |>
    dplyr::arrange(dplyr::desc(varde)) |>
    utils::head(n) |>
    dplyr::pull(naringsgren_kod)
}

# ---- Behov & rekrytering --------------------------------------------------------

# SCB:s bedömning av arbetsmarknadsläget 2040, i ordning från störst brist.
# Samma kategorier och färger som Utbildning-appen (def_farger.R där:
# REGTOP_BEDOMNINGAR/BEDOMNING_FARGER) -- håll dem i synk.
REGTOP_BEDOMNINGAR <- c("Stor brist", "Brist", "Viss brist", "Balans",
                        "Visst \u00f6verskott", "\u00d6verskott", "Stort \u00f6verskott")
BEDOMNING_SAKNAS   <- "Prognos saknas"
BEDOMNING_BRIST    <- REGTOP_BEDOMNINGAR[1:3]
BEDOMNING_OVERSKOTT <- REGTOP_BEDOMNINGAR[5:7]

# Yrken (RegTop:s yrkesgrupper, 5 siffror) med färre förvärvsarbetande i
# länet än så här tas inte med i topplistan över bristberoende yrken.
BEHOV_MIN_YRKE <- 200

# Yrkesgrupper som betyder att yrke saknas.
REGTOP_YRKE_SAKNAS <- c("99998", "99999")

# Bedömning 2040 per utbildningsgrupp i länet. Grupper utan framskrivning
# får "Prognos saknas".
hamta_bedomning_lan <- function() {
  .regtop_hamta("bedomning_2040") |>
    dplyr::filter(regionkod == REGTOP_LAN) |>
    dplyr::transmute(
      utbildningsgrupp_kod,
      bedomning = dplyr::if_else(framskrivning_2040 %in% "Ja" &
                                   bedomning_2040 %in% REGTOP_BEDOMNINGAR,
                                 bedomning_2040, BEDOMNING_SAKNAS),
      balanstal = as.numeric(balanstal_procent)
    )
}

# Utbildningsstruktur per yrke i länet 2023 (andel 0-1 av yrkets
# förvärvsarbetande per utbildningsgrupp), med bedömning 2040.
hamta_yrke_utbildning_bedomning <- function() {
  .regtop_hamta("yrke_utbildning_andel") |>
    dplyr::filter(regionkod == REGTOP_LAN, andel_av == "yrke") |>
    dplyr::transmute(yrke_kod, yrke, utbildningsgrupp_kod, utbildningsgrupp,
                     andel = varde / 100) |>
    dplyr::left_join(hamta_bedomning_lan(), by = "utbildningsgrupp_kod") |>
    dplyr::mutate(bedomning = dplyr::coalesce(bedomning, BEDOMNING_SAKNAS),
                  bedomning = factor(bedomning, levels = c(REGTOP_BEDOMNINGAR, BEDOMNING_SAKNAS)))
}

# Förvärvsarbetande per yrkesgrupp i länet 2023 (yrkesstruktur), utan
# "yrke saknas".
hamta_yrken_lan <- function() {
  .regtop_hamta("yrkesstruktur") |>
    dplyr::filter(regionkod == REGTOP_LAN, enhet == "antal",
                  !yrke_kod %in% REGTOP_YRKE_SAKNAS) |>
    dplyr::group_by(yrke_kod, yrke) |>
    dplyr::summarise(antal = sum(varde), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(antal))
}

# Andel av varje yrkes förvärvsarbetande per bedömning (brist/balans/...),
# med enhet_kod/enhet som i bransch_andel_per_bedomning().
yrke_andel_per_bedomning <- function(yub) {
  yub |>
    dplyr::group_by(enhet_kod = yrke_kod, enhet = yrke, bedomning, .drop = FALSE) |>
    dplyr::summarise(andel = sum(andel), .groups = "drop")
}

# RegTop-yrkesgrupp som motsvarar en SSYK 2012-kod på tresiffernivå
# (från mikrodatan i Utbildning & yrken), via nyckel_yrkesgrupper
# (SSYK fyrsiffrig). Flera träffar -> den största i länet. NULL om ingen.
regtop_yrke_for_ssyk3 <- function(ssyk3, yrken_lan) {
  if (is.null(ssyk3) || !nzchar(ssyk3)) return(NULL)
  grupper <- .regtop_hamta("nyckel_yrkesgrupper", region = FALSE) |>
    dplyr::filter(substr(as.character(yrke_kod), 1, 3) == ssyk3) |>
    dplyr::pull(yrkesgrupp_kod) |>
    unique()
  traff <- yrken_lan$yrke_kod[yrken_lan$yrke_kod %in% grupper]
  if (length(traff) == 0) NULL else traff[1]  # yrken_lan är sorterad efter storlek
}

# ---- Bedömning för branscher (mikrodata + RegTop) -----------------------------
# Mikrodatans utbildningsgrupp (sun2020grp_t23 i utb_yrken_branscher)
# kopplas till RegTop:s utbildningsgrupper (sun2020grp_alt_reg, samma koder
# som i bedomning_2040) via regtop.nyckel_utbildningsgrupper.

BEDOMNING_INGAR_INTE <- "Ingår inte i bedömningen"
BEDOMNING_NIVAER <- c(REGTOP_BEDOMNINGAR, BEDOMNING_SAKNAS, BEDOMNING_INGAR_INTE)

hamta_nyckel_utbildningsgrupper <- function() {
  .regtop_hamta("nyckel_utbildningsgrupper", region = FALSE) |>
    dplyr::distinct(sun2020grp_t23, sun2020grp_alt_reg, kortnamn_alt_reg)
}

# Sysselsatta per bransch (grupp i vald indelning) och RegTop-
# utbildningsgrupp i vald geografi och år, med SCB:s bedömning 2040 för
# Dalarnas län. Alla branscher i indelningen. Kolumner: bransch,
# utbildningsgrupp_kod, utbildningsgrupp, bedomning, balanstal, antal.
hamta_bransch_utbildning_bedomning <- function(ar_val, geografi, indelning_kolumn) {
  dim_br <- hamta_dim_bransch() |>
    dplyr::select(branschkod, bransch = dplyr::all_of(indelning_kolumn)) |>
    dplyr::filter(!is.na(bransch), bransch != "")
  bed <- hamta_bedomning_lan()

  hamta_yrke_utb(ar_val, geografi, c("branschkod", "sun2020grp_t23")) |>
    dplyr::mutate(branschkod = sprintf("%02d", as.integer(branschkod)),
                  sun2020grp_t23 = as.character(sun2020grp_t23)) |>
    dplyr::inner_join(dim_br, by = "branschkod") |>
    dplyr::left_join(hamta_nyckel_utbildningsgrupper(), by = "sun2020grp_t23") |>
    dplyr::left_join(bed, by = c("sun2020grp_alt_reg" = "utbildningsgrupp_kod")) |>
    dplyr::mutate(
      utbildningsgrupp_kod = dplyr::coalesce(sun2020grp_alt_reg, "saknas"),
      utbildningsgrupp = dplyr::coalesce(kortnamn_alt_reg, "Uppgift saknas"),
      bedomning = factor(dplyr::coalesce(bedomning, BEDOMNING_INGAR_INTE), levels = BEDOMNING_NIVAER)
    ) |>
    dplyr::group_by(bransch, utbildningsgrupp_kod, utbildningsgrupp, bedomning, balanstal) |>
    dplyr::summarise(antal = sum(antal), .groups = "drop")
}

# Andelar för ett urval branscher (NULL = alla): per utbildningsgrupp
# (andel av sysselsatta) -- samma kolumner som skapa_diagram_utbildningar()
# vill ha.
bransch_utbildningar <- function(bub, branscher = NULL) {
  d <- if (length(branscher) > 0) dplyr::filter(bub, bransch %in% branscher) else bub
  d |>
    dplyr::group_by(utbildningsgrupp_kod, utbildningsgrupp, bedomning, balanstal) |>
    dplyr::summarise(antal = sum(antal), .groups = "drop") |>
    dplyr::mutate(andel = antal / sum(antal))
}

# Andel av varje branschs sysselsatta per bedömning (för topplistan).
bransch_andel_per_bedomning <- function(bub) {
  bub |>
    dplyr::group_by(enhet = bransch, bedomning, .drop = FALSE) |>
    dplyr::summarise(n = sum(antal), .groups = "drop") |>
    dplyr::group_by(enhet) |>
    dplyr::mutate(antal = sum(n), andel = dplyr::if_else(antal > 0, n / antal, 0)) |>
    dplyr::ungroup() |>
    dplyr::transmute(enhet_kod = enhet, enhet, antal, bedomning, andel)
}
