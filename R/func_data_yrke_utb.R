# ============================================================
#  func_data_yrke_utb.R
#  Dataåtkomst för fliken "Utbildning & yrken".
#
#  Källa (databasen "oppna_data"): mikro_db.utb_yrken_branscher.
#  Kolumner: ar, bas (1 = sysselsatt), bas_namn, ssyk3_2012,
#  ssyk3_2012_namn, sun2020niva(_namn), sun2020grp(_namn),
#  sun2020grp_t23(_klartext), sun2000grp_t20(_klartext), alder, kon,
#  bakgrund, match_23, matchningsindikator, gruppering, regionkod_ast,
#  region_ast, regionkod_bo, region_bo, branschkod, bransch,
#  syss (= antal).
#
#  Till skillnad från syss_branscher läses tabellen INTE in i sin helhet
#  -- den är för stor. Varje anrop filtrerar (år, regionkod_ast, bransch)
#  och summerar i databasen, och bara det aggregerade resultatet hämtas.
#  Rekommenderat index i databasen:
#    CREATE INDEX ON mikro_db.utb_yrken_branscher (ar, regionkod_ast, branschkod);
#
#  Precis som i syss_branscher finns inga "totalt"-rader: riket ("00"),
#  länet ("20") och kommuner ligger som egna rader, och ålder/kön/
#  bakgrund/bosättningsregion summeras bort genom att inte grupperas på.
# ============================================================

YRKE_UTB_TABELL <- c(schema = "mikro_db", tabell = "utb_yrken_branscher")

KALLA_YRKE_UTB <- "SCB (Yrkesregistret, Utbildningsregistret), bearbetat av Region Dalarna"

# Utbildningsindelningar som går att välja i UI:t, i den ordning de
# visas. Den första (T23) är förvald.
UTB_INDELNINGAR <- tibble::tribble(
  ~namn,                                   ~kod_kol,          ~namn_kol,
  "Utbildningsgrupp (SUN 2020, T23)",      "sun2020grp_t23",  "sun2020grp_t23_klartext",
  "Utbildningsgrupp (SUN 2000, T20)",      "sun2000grp_t20",  "sun2000grp_t20_klartext",
  "Utbildningsgrupp (SUN 2020)",           "sun2020grp",      "sun2020grp_namn",
  "Utbildningsnivå (SUN 2020)",            "sun2020niva",     "sun2020niva_namn"
)

# Koder/namn som betyder okänt -- väljs inte som förval i listorna.
.ar_okand <- function(kod, namn) {
  kod %in% c("***", "saknas") | grepl("saknas|ok\u00e4nd", namn, ignore.case = TRUE)
}

# Andelar visas bara när nämnaren (t.ex. ett yrkes sysselsatta) är minst
# så här stor -- skydd mot att små celler kan räknas fram ur andelar.
MIN_NAMNARE <- 20

.utb_namn_kol <- function(kod_kol) {
  UTB_INDELNINGAR$namn_kol[UTB_INDELNINGAR$kod_kol == kod_kol]
}

# Matchning ("gruppering"). Andelar matchade räknas bland de tre
# MATCHNING_GRUPPER; MATCHNING_UTAN visas separat (andel av anställda).
# Övriga värden ("Ingår inte", "Visas inte") tas inte med.
MATCHNING_GRUPPER <- c("Helt matchade", "Delvis matchade", "Inte matchade")
MATCHNING_UTAN    <- "Anst\u00e4llda utan tillr\u00e4ckliga uppgifter"

.yrke_utb_cache <- new.env(parent = emptyenv())

.yrke_utb_tbl <- function(con) {
  dplyr::tbl(con, dbplyr::in_schema(YRKE_UTB_TABELL[["schema"]],
                                    YRKE_UTB_TABELL[["tabell"]]))
}

# Tillgängliga år i tabellen, nyast först. Hämtas en gång per process.
hamta_ar_lista_yrke_utb <- function(force = FALSE) {
  if (force || is.null(.yrke_utb_cache$ar)) {
    con <- shiny_uppkoppling_las("oppna_data")
    on.exit(DBI::dbDisconnect(con))
    .yrke_utb_cache$ar <- .yrke_utb_tbl(con) |>
      dplyr::distinct(ar) |>
      dplyr::collect() |>
      dplyr::pull(ar) |>
      as.integer() |>
      sort(decreasing = TRUE)
  }
  .yrke_utb_cache$ar
}

# Koder i den typ kolumnen har i databasen (tal eller text), så att
# filtret jämför kolumnen direkt -- en CAST på kolumnen gör att indexet
# inte används. Kolumntyperna läses en gång (fråga som ger 0 rader).
.som_kolumntyp <- function(con, kol, koder) {
  if (is.null(.yrke_utb_cache$typer)) {
    .yrke_utb_cache$typer <- .yrke_utb_tbl(con) |>
      utils::head(0) |>
      dplyr::collect() |>
      vapply(function(x) class(x)[1], character(1))
  }
  if (.yrke_utb_cache$typer[[kol]] %in% c("integer", "numeric", "integer64")) {
    as.integer(koder)
  } else {
    as.character(koder)
  }
}

# Grundfråga: summa sysselsatta grupperat på regionkod_ast + dims, för
# valt år, valda geografier och (valfritt) valda branschkoder. villkor är
# en namngiven lista med likhetsfilter, t.ex. list(ssyk3_2012 = "251").
hamta_yrke_utb <- function(ar_val, geografier, dims, branschkoder = NULL,
                           villkor = list()) {
  con <- shiny_uppkoppling_las("oppna_data")
  on.exit(DBI::dbDisconnect(con))

  q <- .yrke_utb_tbl(con) |>
    dplyr::filter(
      ar  == !!as.integer(ar_val),
      bas == 1L,  # sysselsatta
      regionkod_ast %in% !!.som_kolumntyp(con, "regionkod_ast", geografier)
    )

  if (length(branschkoder) > 0) {
    q <- dplyr::filter(q, branschkod %in% !!.som_kolumntyp(con, "branschkod", branschkoder))
  }
  for (kol in names(villkor)) {
    q <- dplyr::filter(q, !!rlang::sym(kol) == !!villkor[[kol]])
  }

  q |>
    dplyr::group_by(regionkod_ast, !!!rlang::syms(dims)) |>
    dplyr::summarise(antal = sum(syss, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |>
    dplyr::mutate(
      kommun_kod = sprintf("%02d", as.integer(regionkod_ast)),
      antal      = as.numeric(antal)
    ) |>
    dplyr::select(-regionkod_ast)
}

# Branschkoder (SNI 2-siffrigt) som hör till en grupp i en indelning.
# grupp_namn = "" (Alla branscher) ger NULL, dvs. inget branschfilter.
hamta_branschkoder <- function(indelning_kolumn, grupp_namn) {
  if (is.null(grupp_namn) || !nzchar(grupp_namn)) return(NULL)
  hamta_dim_bransch() |>
    dplyr::filter(.data[[indelning_kolumn]] == grupp_namn) |>
    dplyr::pull(branschkod)
}

# Yrke x utbildning(sgrupp) för vald geografi och riket. Grund för
# nyckeltal och båda mosaikerna (yrke -> utbildningar och utbildning ->
# yrken). Returnerar kolumnerna kommun_kod, yrke_kod, yrke_namn,
# utb_kod, utb_namn, antal.
hamta_yrke_x_utb <- function(ar_val, geografi, branschkoder, utb_kol) {
  namn_kol <- .utb_namn_kol(utb_kol)
  dims <- c("ssyk3_2012", "ssyk3_2012_namn", utb_kol, stats::na.omit(namn_kol))

  df <- hamta_yrke_utb(ar_val, unique(c(geografi, "00")), dims, branschkoder)

  df |>
    dplyr::transmute(
      kommun_kod,
      yrke_kod  = .kod_eller_saknas(ssyk3_2012),
      yrke_namn = .namn_eller_kod(ssyk3_2012_namn, yrke_kod),
      utb_kod   = .kod_eller_saknas(.data[[utb_kol]]),
      utb_namn  = .namn_eller_kod(if (is.na(namn_kol)) NA_character_ else .data[[namn_kol]],
                                  utb_kod),
      antal
    ) |>
    # Koder som saknar namn kan ge flera rader per kod -- slå ihop.
    dplyr::group_by(kommun_kod, yrke_kod, yrke_namn, utb_kod, utb_namn) |>
    dplyr::summarise(antal = sum(antal), .groups = "drop")
}

# Alla koder och namn behöver ett värde -- NA ger fel i väljarna
# (updateSelectizeInput) och tomma rutor i diagrammen.
.kod_eller_saknas <- function(kod) {
  dplyr::coalesce(as.character(kod), "saknas")
}
.namn_eller_kod <- function(namn, kod) {
  namn <- dplyr::if_else(is.na(namn) | namn == "", kod, as.character(namn))
  dplyr::if_else(namn == "saknas", "Uppgift saknas", namn)
}

# Yrke x valda kategorikolumner (t.ex. gruppering, alder, kon, bakgrund)
# för vald geografi. Grund för fördelningsdiagrammen per yrke i flikarna
# Matchning och Demografi. Enheten (yrket) heter enhet_kod/enhet_namn så
# att samma bearbetning och diagram fungerar för branscher (se nedan).
hamta_yrke_profil <- function(ar_val, geografi, branschkoder, kat_kolumner) {
  hamta_yrke_utb(ar_val, geografi, c("ssyk3_2012", "ssyk3_2012_namn", kat_kolumner),
                 branschkoder) |>
    dplyr::mutate(
      enhet_kod  = .kod_eller_saknas(ssyk3_2012),
      enhet_namn = .namn_eller_kod(ssyk3_2012_namn, enhet_kod)
    ) |>
    dplyr::select(enhet_kod, enhet_namn, dplyr::all_of(kat_kolumner), antal)
}

# Bransch (grupp i vald indelning) x valda kategorikolumner för vald
# geografi -- alla branscher, inte filtrerat på vald bransch. Samma
# kolumner som hamta_yrke_profil(); enhet_kod = enhet_namn = gruppnamnet.
hamta_bransch_profil <- function(ar_val, geografi, indelning_kolumn, kat_kolumner) {
  dim_br <- hamta_dim_bransch() |>
    dplyr::select(branschkod, grupp = dplyr::all_of(indelning_kolumn)) |>
    dplyr::filter(!is.na(grupp), grupp != "")

  hamta_yrke_utb(ar_val, geografi, c("branschkod", kat_kolumner)) |>
    dplyr::mutate(branschkod = sprintf("%02d", as.integer(branschkod))) |>
    dplyr::inner_join(dim_br, by = "branschkod") |>
    dplyr::group_by(enhet_kod = grupp, enhet_namn = grupp,
                    dplyr::across(dplyr::all_of(kat_kolumner))) |>
    dplyr::summarise(antal = sum(antal), .groups = "drop")
}

# Andel av totalen i en profil-df där kat_kol har något av värdena.
# NA om underlaget är mindre än MIN_NAMNARE.
andel_av_total <- function(profil, kat_kol, varden, bland = NULL) {
  d <- if (is.null(bland)) profil else dplyr::filter(profil, .data[[kat_kol]] %in% bland)
  tot <- sum(d$antal)
  if (tot < MIN_NAMNARE) return(NA_real_)
  sum(d$antal[d[[kat_kol]] %in% varden]) / tot
}

# ---- Bearbetning (i R, på redan hämtad data) ------------------------------

# De n största enheterna (yrken/branscher) efter antal sysselsatta.
# Okänt yrke ("***"/"saknas") tas inte med.
storsta_enheter <- function(profil, n = 20) {
  profil |>
    dplyr::group_by(enhet_kod, enhet_namn) |>
    dplyr::summarise(antal = sum(antal), .groups = "drop") |>
    dplyr::filter(!enhet_kod %in% c("***", "saknas")) |>
    dplyr::slice_max(antal, n = n, with_ties = FALSE)
}

# Fördelning över en kategorikolumn per enhet, för de n största.
# Andelar för enheter med färre än MIN_NAMNARE sysselsatta sätts till NA.
fordelning_per_enhet <- function(profil, kat_kol, n = 20) {
  topp <- storsta_enheter(profil, n)

  profil |>
    dplyr::semi_join(topp, by = "enhet_kod") |>
    dplyr::group_by(enhet_kod, enhet_namn, kategori = .data[[kat_kol]]) |>
    dplyr::summarise(antal = sum(antal), .groups = "drop") |>
    dplyr::group_by(enhet_kod) |>
    dplyr::mutate(
      total = sum(antal),
      andel = dplyr::if_else(total >= MIN_NAMNARE, antal / total, NA_real_)
    ) |>
    dplyr::ungroup()
}

# Andel per kategori inom en vald enhet (ett yrke eller en utbildning),
# för vald geografi och riket. kat = "utb" ger utbildningar inom ett
# yrke, kat = "yrke" ger yrken inom en utbildning. De topp_n största
# (i vald geografi) visas, resten samlas i "Övriga".
andel_inom <- function(yrke_x_utb, geografi, filter_kol, filter_varde,
                       kat = c("utb", "yrke"), topp_n = 15) {
  kat <- match.arg(kat)
  kod_kol  <- paste0(kat, "_kod")
  namn_kol <- paste0(kat, "_namn")

  d <- yrke_x_utb |>
    dplyr::filter(.data[[filter_kol]] == filter_varde) |>
    # Är vald geografi riket blir allt "vald" (ingen separat jämförelse).
    dplyr::mutate(geo_niva = dplyr::if_else(kommun_kod == geografi, "vald", "riket")) |>
    dplyr::group_by(geo_niva, kod = .data[[kod_kol]], namn = .data[[namn_kol]]) |>
    dplyr::summarise(antal = sum(antal), .groups = "drop")

  topp <- d |>
    dplyr::filter(geo_niva == "vald") |>
    dplyr::slice_max(antal, n = topp_n, with_ties = FALSE) |>
    dplyr::pull(kod)

  d |>
    dplyr::mutate(
      namn = dplyr::if_else(kod %in% topp, namn, "Övriga"),
      kod  = dplyr::if_else(kod %in% topp, kod, "övr")
    ) |>
    dplyr::group_by(geo_niva, kod, namn) |>
    dplyr::summarise(antal = sum(antal), .groups = "drop") |>
    dplyr::group_by(geo_niva) |>
    dplyr::mutate(total = sum(antal), andel = antal / total) |>
    dplyr::ungroup()
}

# Rekryteringsbredd: antal utbildningsgrupper som krävs för att täcka
# 80 % av ett yrkes sysselsatta. Returnerar medianen över yrken med minst
# MIN_NAMNARE sysselsatta (NA om inga sådana finns).
rekryteringsbredd_median <- function(yrke_x_utb, geografi, tackning = 0.8) {
  per_yrke <- yrke_x_utb |>
    dplyr::filter(kommun_kod == geografi, !yrke_kod %in% c("***", "saknas")) |>
    dplyr::group_by(yrke_kod) |>
    dplyr::filter(sum(antal) >= MIN_NAMNARE) |>
    dplyr::arrange(dplyr::desc(antal), .by_group = TRUE) |>
    dplyr::summarise(bredd = which(cumsum(antal) / sum(antal) >= tackning)[1])

  if (nrow(per_yrke) == 0) return(NA_real_)
  stats::median(per_yrke$bredd)
}

# Andel anställda utan tillräckliga yrkes-/utbildningsuppgifter per
# bransch (alla branscher i vald indelning -- inte filtrerat på vald
# bransch), för vald geografi och riket. Returnerar samma kolumner som
# andel_inom(), så att skapa_diagram_andel_inom() kan rita den.
hamta_andel_utan_uppgifter <- function(ar_val, geografi, indelning_kolumn) {
  dim_br <- hamta_dim_bransch() |>
    dplyr::select(branschkod, namn = dplyr::all_of(indelning_kolumn)) |>
    dplyr::filter(!is.na(namn), namn != "")

  hamta_yrke_utb(ar_val, unique(c(geografi, "00")), c("branschkod", "gruppering")) |>
    dplyr::filter(gruppering %in% c(MATCHNING_GRUPPER, MATCHNING_UTAN)) |>
    dplyr::mutate(branschkod = sprintf("%02d", as.integer(branschkod))) |>
    dplyr::inner_join(dim_br, by = "branschkod") |>
    dplyr::mutate(geo_niva = dplyr::if_else(kommun_kod == geografi, "vald", "riket")) |>
    dplyr::group_by(geo_niva, kod = namn, namn) |>
    dplyr::summarise(
      total = sum(antal),
      antal = sum(antal[gruppering == MATCHNING_UTAN]),
      .groups = "drop"
    ) |>
    dplyr::filter(total >= MIN_NAMNARE) |>
    dplyr::mutate(andel = antal / total)
}

# ---- Åldersgrupper ----------------------------------------------------------

# Undre/övre gräns för åldersgrupper som "20-29 år" eller "68+ år"
# (övre = Inf). Grupper som inte går att tolka får NA.
.alder_granser <- function(grupp) {
  m <- regmatches(grupp, regexec("(\\d+)\\s*[-–]\\s*(\\d+)|(\\d+)\\s*\\+", grupp))
  g <- vapply(m, function(x) {
    if (length(x) == 0) c(NA_real_, NA_real_)
    else if (nzchar(x[2])) as.numeric(x[2:3])
    else c(as.numeric(x[4]), Inf)
  }, numeric(2))
  matrix(g, ncol = 2, byrow = TRUE)
}

# Åldersgrupper i åldersordning (yngst först).
sortera_aldersgrupper <- function(grupper) {
  grupper <- unique(grupper)
  grupper[order(.alder_granser(grupper)[, 1], grupper)]
}

# Etikett för valda åldersgrupper där intilliggande grupper slås ihop:
# "20-29 år" + "30-39 år" + "40-49 år" -> "20–49 år", "60-67" + "68+"
# -> "60+ år", ej intilliggande -> "20–29 och 60–67 år".
alder_etikett <- function(valda, alla) {
  alla <- sortera_aldersgrupper(alla)
  pos  <- sort(match(valda, alla))
  g    <- .alder_granser(alla)
  if (length(pos) == 0) return("")
  if (anyNA(g[pos, ])) return(paste(valda, collapse = ", "))

  delar <- vapply(split(pos, cumsum(c(1, diff(pos) != 1))), function(p) {
    lo <- g[p[1], 1]; hi <- g[p[length(p)], 2]
    if (is.infinite(hi)) paste0(lo, "+") else paste0(lo, "–", hi)
  }, character(1))

  lista <- if (length(delar) == 1) delar
           else paste(paste(utils::head(delar, -1), collapse = ", "), "och", utils::tail(delar, 1))
  paste(lista, "år")
}

# Förvald åldersgrupp för sortering: 68+ om den finns, annars äldsta.
alder_forval <- function(alla) {
  alla <- sortera_aldersgrupper(alla)
  g <- .alder_granser(alla)
  aldst <- alla[!is.na(g[, 1]) & g[, 1] >= 68]
  if (length(aldst) > 0) aldst else utils::tail(alla, 1)
}
