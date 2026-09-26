# ============================================================
#  func_ui.R
#  Delade UI-byggstenar.
# ============================================================

# Nyckeltalskort (rd-kpi) med valfri förklaring. Förklaringen visas när
# man hovrar över kortet eller ger det fokus (tab/tryck på mobil); en
# liten i-ikon vid etiketten visar att den finns. Ren CSS (app.css), inga
# JS-bibliotek.
rd_kpi <- function(etikett, varde, forklaring = NULL) {
  if (is.null(forklaring)) {
    return(div(class = 'rd-kpi',
               div(class = 'rd-kpi__label', etikett),
               div(class = 'rd-kpi__value', varde)))
  }
  div(class = 'rd-kpi rd-kpi--hjalp', tabindex = '0',
      div(class = 'rd-kpi__label', etikett,
          tags$span(class = 'rd-hjalp', `aria-hidden` = 'true', 'i')),
      div(class = 'rd-kpi__value', varde),
      div(class = 'rd-hjalp__text', role = 'tooltip', forklaring))
}
