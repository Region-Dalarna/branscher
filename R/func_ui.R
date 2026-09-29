# ============================================================
#  func_ui.R
#  Delade UI-byggstenar.
# ============================================================

# Nyckeltalskort (rd-kpi) med valfri förklaring. Förklaringen visas när
# man hovrar över kortet eller ger det fokus (tab/tryck på mobil); en
# liten i-ikon vid etiketten visar att den finns. Ren CSS (app.css), inga
# JS-bibliotek. under: valfri liten rad under värdet (t.ex. vilket urval
# siffran gäller).
rd_kpi <- function(etikett, varde, forklaring = NULL, under = NULL) {
  under <- if (!is.null(under)) div(class = 'rd-kpi__under', under)
  if (is.null(forklaring)) {
    return(div(class = 'rd-kpi',
               div(class = 'rd-kpi__label', etikett),
               div(class = 'rd-kpi__value', varde),
               under))
  }
  div(class = 'rd-kpi rd-kpi--hjalp', tabindex = '0',
      div(class = 'rd-kpi__label', etikett,
          tags$span(class = 'rd-hjalp', `aria-hidden` = 'true', 'i')),
      div(class = 'rd-kpi__value', varde),
      under,
      div(class = 'rd-hjalp__text', role = 'tooltip', forklaring))
}

# Sökväg till en fil i www/ med versionsnyckel (?v=<md5>), så att
# webbläsaren hämtar om filen när den ändrats i stället för att använda
# en gammal cachad version.
www_version <- function(fil) {
  paste0(fil, '?v=', substr(unname(tools::md5sum(file.path('www', fil))), 1, 8))
}

# Yrkesväljare för diagram över de största yrkena: sökruta där man lägger
# till yrken (input `yrken_val`) och kryssruta "Visa bara valda yrken"
# (input `bara_valda`). Utan kryss läggs valda yrken till bland de
# största och framhävs; med kryss visas bara de valda.
yrkesval_ui <- function(ns, placeholder = 'S\u00f6k och l\u00e4gg till yrken\u2026', class = NULL) {
  div(class = paste(c('rd-yrkesval', class), collapse = ' '),
      div(class = 'rd-yrkesval__sok',
          selectizeInput(ns('yrken_val'), NULL, choices = NULL, multiple = TRUE, width = '100%',
                         options = list(plugins = list('remove_button'),
                                        sortField = '$order',
                                        placeholder = placeholder))),
      div(class = 'rd-yrkesval__bara',
          checkboxInput(ns('bara_valda'), 'Visa bara valda yrken', value = FALSE)))
}
