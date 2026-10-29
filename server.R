shinyServer(function(input, output, session) {

  telemetri_server(telemetry, navigation_id = 'flik', forsta_flik = 'Översikt')

  # ---- Djuplänkning: läs ev. query-parametrar i URL:en en gång vid start.
  # flik=<slug> väljer flik direkt (se R/def_lankning.R för slugs); övriga
  # parametrar tolkas av respektive modul (se R/mod_oversikt.R).
  lank <- shiny::isolate(shiny::parseQueryString(session$clientData$url_search))

  if (!is.null(lank$flik) && lank$flik %in% names(FLIK_SLUGS)) {
    shiny::updateTabsetPanel(session, 'flik', selected = FLIK_SLUGS[[lank$flik]])
  }

  # Håll adressfältet synkat med vald flik, så att den går att dela utan
  # någon extra "kopiera länk"-knapp (ignoreInit så vi inte skriver över
  # en fullständig diagramlänk direkt vid sidladdning).
  shiny::observeEvent(input$flik, ignoreInit = TRUE, {
    slug <- names(FLIK_SLUGS)[match(input$flik, FLIK_SLUGS)]
    if (!is.na(slug)) shiny::updateQueryString(paste0('?flik=', slug), mode = 'replace')
  })

  # Övriga flikar är platshållare utan server tills vidare.
  mod_oversikt_server('oversikt', lank = lank)
  # Flikarna nedan hämtar data först när de öppnas (aktiv).
  utb_yrken <- mod_utbildning_yrken_server('utbildning_yrken',
                                          aktiv = reactive(input$flik == 'Utbildning & yrken'))
  mod_matchning_server('matchning', aktiv = reactive(input$flik == 'Matchning'))
  mod_demografi_server('demografi', aktiv = reactive(input$flik == 'Demografi'))
  mod_prognos_server('prognos', aktiv = reactive(input$flik == 'Prognos'))
  mod_behov_server('behov', aktiv = reactive(input$flik == 'Behov & rekrytering'),
                   extern_yrke = utb_yrken$yrke)

})
