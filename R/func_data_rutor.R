# ============================================================
#  func_data_rutor.R
#  Dataåtkomst för fliken "Geografi".
#
#  Källa (databasen "sekretess"): rutor.syssrutor_100m. En rad per
#  100-metersruta (arbetsställets läge, geom i SWEREF99 TM) x bransch
#  (SNI 5-siffrigt) x yrke (SSYK 4-siffrigt), med antal sysselsatta.
#  branschkod är ett heltal (inledande nolla saknas, 02101 lagras som 2101),
#  så SNI-huvudgruppen räknas fram som floor(branschkod / 1000).
#
#  Allt aggregeras i databasen till 1000-metersrutor innan collect(), så
#  appen hämtar aldrig mikrodata -- bara antal per km-ruta för valt urval.
#  Rutans hörn räknas fram från 100-metersrutans mittpunkt, avrundad nedåt
#  till närmaste 1000 m.
# ============================================================

RUTOR_TABELL <- c(schema = "rutor", tabell = "syssrutor_100m")

RUTSTORLEK       <- 1000  # meter, SWEREF99 TM
RUTA_LITEN_GRANS <- 4     # rutor med färre sysselsatta visas som en egen, ljus klass
URVAL_MIN_SYSS   <- 30    # minsta antal sysselsatta i länet för att en bransch/ett yrke ska gå att välja
YRKE_SAKNAS      <- "****"

KALLA_RUTOR <- "SCB, sysselsatta efter arbetsställets läge (kilometerrutor), bearbetat av Region Dalarna"

.rutor_cache <- new.env(parent = emptyenv())

.rutor_uppkoppling <- function() {
  con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till databasen sekretess.")
  con
}

.rutor_tbl <- function(con) {
  dplyr::tbl(con, dbplyr::in_schema(RUTOR_TABELL[["schema"]], RUTOR_TABELL[["tabell"]])) |>
    dplyr::filter(astlanskod == "20")
}

.rutor_cachat <- function(nyckel, hamta) {
  if (is.null(.rutor_cache[[nyckel]])) .rutor_cache[[nyckel]] <- hamta()
  .rutor_cache[[nyckel]]
}

hamta_ar_lista_rutor <- function() {
  .rutor_cachat("ar", function() {
    con <- .rutor_uppkoppling()
    on.exit(DBI::dbDisconnect(con))
    ar <- .rutor_tbl(con) |> dplyr::distinct(ar) |> dplyr::collect()
    sort(as.integer(ar$ar), decreasing = TRUE)
  })
}

# Sysselsatta i länet per SNI-huvudgrupp (2 siffror), valt år.
hamta_rutor_bransch_totaler <- function(ar_val) {
  ar_val <- as.integer(ar_val)
  .rutor_cachat(paste0("bransch_tot_", ar_val), function() {
    con <- .rutor_uppkoppling()
    on.exit(DBI::dbDisconnect(con))
    .rutor_tbl(con) |>
      dplyr::filter(ar == !!ar_val) |>
      dplyr::mutate(sni2 = floor(branschkod / 1000)) |>
      dplyr::group_by(sni2) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      dplyr::mutate(sni2 = sprintf("%02d", as.integer(sni2)), antal = as.numeric(antal))
  })
}

# Sysselsatta i länet per yrke (SSYK 4-siffrigt), valt år.
hamta_rutor_yrke_totaler <- function(ar_val) {
  ar_val <- as.integer(ar_val)
  .rutor_cachat(paste0("yrke_tot_", ar_val), function() {
    con <- .rutor_uppkoppling()
    on.exit(DBI::dbDisconnect(con))
    .rutor_tbl(con) |>
      dplyr::filter(ar == !!ar_val, yrkeskod != !!YRKE_SAKNAS, !is.na(yrke)) |>
      dplyr::group_by(yrkeskod, yrke) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      dplyr::mutate(antal = as.numeric(antal))
  })
}

# Branschgrupper i vald indelning med minst URVAL_MIN_SYSS sysselsatta i
# länet. Kolumner: grupp_namn, antal, sni2 (list: ingående SNI-koder).
rutor_valbara_branscher <- function(ar_val, indelning_kolumn) {
  dim_br <- hamta_dim_bransch() |>
    dplyr::select(branschkod, grupp_namn = dplyr::all_of(indelning_kolumn)) |>
    dplyr::filter(!is.na(grupp_namn), grupp_namn != "")
  hamta_rutor_bransch_totaler(ar_val) |>
    dplyr::inner_join(dim_br, by = c(sni2 = "branschkod")) |>
    dplyr::group_by(grupp_namn) |>
    dplyr::summarise(antal = sum(antal), sni2 = list(sni2), .groups = "drop") |>
    dplyr::filter(antal >= URVAL_MIN_SYSS) |>
    dplyr::arrange(grupp_namn)
}

rutor_valbara_yrken <- function(ar_val) {
  hamta_rutor_yrke_totaler(ar_val) |>
    dplyr::filter(antal >= URVAL_MIN_SYSS) |>
    dplyr::arrange(yrke)
}

# Sysselsatta per 1000-metersruta. sni2/yrkeskoder = NULL ger alla.
# Kolumner: rut_x, rut_y (rutans sydvästra hörn, SWEREF99 TM), antal.
hamta_rutor_km <- function(ar_val, sni2 = NULL, yrkeskoder = NULL) {
  ar_val <- as.integer(ar_val)
  nyckel <- paste("km", ar_val, paste(sort(sni2), collapse = ","),
                  paste(sort(yrkeskoder), collapse = ","), sep = "|")
  .rutor_cachat(nyckel, function() {
    con <- .rutor_uppkoppling()
    on.exit(DBI::dbDisconnect(con))
    q <- .rutor_tbl(con) |> dplyr::filter(ar == !!ar_val)
    if (!is.null(sni2))       q <- dplyr::filter(q, floor(branschkod / 1000) %in% !!as.integer(sni2))
    if (!is.null(yrkeskoder)) q <- dplyr::filter(q, yrkeskod %in% !!yrkeskoder)
    q |>
      dplyr::mutate(
        rut_x = floor(ST_X(ST_Centroid(geom)) / !!RUTSTORLEK) * !!RUTSTORLEK,
        rut_y = floor(ST_Y(ST_Centroid(geom)) / !!RUTSTORLEK) * !!RUTSTORLEK
      ) |>
      dplyr::group_by(rut_x, rut_y) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      dplyr::mutate(dplyr::across(c(rut_x, rut_y, antal), as.numeric))
  })
}

# Rutornas sydvästra och nordöstra hörn i WGS84, för leaflet::addRectangles().
# Rektanglar i stället för sf-polygoner: många gånger snabbare att skicka
# till kartan, och skevheten mot SWEREF-rutnätet är bara några tiotal meter.
rutor_till_wgs84 <- function(df) {
  sv <- sf::sf_project("EPSG:3006", "EPSG:4326", cbind(df$rut_x, df$rut_y))
  no <- sf::sf_project("EPSG:3006", "EPSG:4326", cbind(df$rut_x + RUTSTORLEK, df$rut_y + RUTSTORLEK))
  dplyr::mutate(df, lng1 = sv[, 1], lat1 = sv[, 2], lng2 = no[, 1], lat2 = no[, 2])
}
