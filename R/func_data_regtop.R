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

.regtop_hamta <- function(tabell, force = FALSE) {
  if (force || is.null(.regtop_cache[[tabell]])) {
    con <- shiny_uppkoppling_las("oppna_data")
    on.exit(DBI::dbDisconnect(con))
    .regtop_cache[[tabell]] <- dplyr::tbl(con, dbplyr::in_schema("regtop", tabell)) |>
      dplyr::filter(regionkod %in% !!REGTOP_REGIONER) |>
      dplyr::collect() |>
      dplyr::mutate(ar = as.integer(ar), varde = as.numeric(varde))
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
