# =====================================================================
# Datafunktioner för flik 1 (Översikt).
#
# OBS: Alla tabell- och kolumnnamn nedan är PÅHITTADE PLACEHOLDERS.
# Ersätts när ETL:en mot mikro_db (databasen oppna_data) är på plats.
# Strukturen följer schemat vi skissat: fakta_sysselsatta, dim_sni,
# dim_indelning, dim_indelning_grupp, brygga_sni_grupp.
# =====================================================================

hamta_grupper_for_indelning <- function(indelning_id) {
  tbl(db_pool, in_schema("mikro_db", "dim_indelning_grupp")) |>
    filter(indelning_id == !!indelning_id) |>
    arrange(sort_ordning) |>
    collect()
}

# Returnerar sysselsatta/etablerade per bransch för tre geografinivåer
# samtidigt ("vald", "lan", "riket") så att jämförelsediagrammet kan
# rita alla tre utan tre separata anrop från UI-lagret.
#
# geografi: "dalarna" (summerar alla Dalarnas kommuner) eller en
# specifik kommun_kod.
hamta_oversiktsdata <- function(indelning_id, geografi) {

  bas <- tbl(db_pool, in_schema("mikro_db", "fakta_sysselsatta")) |>
    filter(kon == "totalt", alder_grupp == "totalt", fodelse_grupp == "totalt") |>
    inner_join(
      tbl(db_pool, in_schema("mikro_db", "brygga_sni_grupp")) |>
        filter(indelning_id == !!indelning_id),
      by = "sni_kod"
    ) |>
    inner_join(
      tbl(db_pool, in_schema("mikro_db", "dim_indelning_grupp")),
      by = "grupp_id"
    )

  kommun_koder_dalarna <- hamta_kommuner()$kommun_kod

  vald_kommuner <- if (geografi == "dalarna") kommun_koder_dalarna else geografi

  vald <- bas |>
    filter(kommun_kod %in% !!vald_kommuner) |>
    group_by(grupp_id, grupp_namn, matt) |>
    summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
    mutate(geografiniva = "vald") |>
    collect()

  lan <- bas |>
    filter(kommun_kod %in% !!kommun_koder_dalarna) |>
    group_by(grupp_id, grupp_namn, matt) |>
    summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
    mutate(geografiniva = "lan") |>
    collect()

  # Riksdata antas ligga i en egen tabell (aggregerad på riksnivå redan
  # vid inläsning, för att slippa summera 290 kommuner varje anrop).
  riket <- tbl(db_pool, in_schema("mikro_db", "fakta_sysselsatta_riket")) |>
    filter(kon == "totalt", alder_grupp == "totalt", fodelse_grupp == "totalt") |>
    inner_join(
      tbl(db_pool, in_schema("mikro_db", "brygga_sni_grupp")) |>
        filter(indelning_id == !!indelning_id),
      by = "sni_kod"
    ) |>
    inner_join(
      tbl(db_pool, in_schema("mikro_db", "dim_indelning_grupp")),
      by = "grupp_id"
    ) |>
    group_by(grupp_id, grupp_namn, matt) |>
    summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
    mutate(geografiniva = "riket") |>
    collect()

  bind_rows(vald, lan, riket) |>
    prick_smavarden(varde_kol = "antal")
}

# --- Sekretess/döljning ---------------------------------------------------

prick_smavarden <- function(df, varde_kol, troskel = 4) {
  df |>
    mutate(
      "{varde_kol}" := if_else(
        .data[[varde_kol]] > 0 & .data[[varde_kol]] < troskel,
        NA_integer_,
        .data[[varde_kol]]
      )
    )
}

formatera_nyckeltal <- function(varde, troskel = 4) {
  if (is.na(varde)) return(glue::glue("F\u00e4rre \u00e4n {troskel}"))
  scales::comma(varde, big.mark = " ")
}
