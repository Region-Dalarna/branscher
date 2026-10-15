shinyServer(function(input, output, session) {

  # Övriga flikar är platshållare utan server tills vidare.
  mod_oversikt_server('oversikt')
  # Flikarna nedan hämtar data först när de öppnas (aktiv).
  utb_yrken <- mod_utbildning_yrken_server('utbildning_yrken',
                                          aktiv = reactive(input$flik == 'Utbildning & yrken'))
  mod_matchning_server('matchning', aktiv = reactive(input$flik == 'Matchning'))
  mod_demografi_server('demografi', aktiv = reactive(input$flik == 'Demografi'))
  mod_prognos_server('prognos', aktiv = reactive(input$flik == 'Prognos'))
  mod_behov_server('behov', aktiv = reactive(input$flik == 'Behov & rekrytering'),
                   extern_yrke = utb_yrken$yrke)

})
