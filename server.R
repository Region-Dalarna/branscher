shinyServer(function(input, output, session) {

  # Övriga flikar är platshållare utan server tills vidare.
  mod_oversikt_server('oversikt')
  # Flikarna nedan hämtar data först när de öppnas (aktiv).
  mod_utbildning_yrken_server('utbildning_yrken',
                              aktiv = reactive(input$flik == 'Utbildning & yrken'))
  mod_matchning_server('matchning', aktiv = reactive(input$flik == 'Matchning'))
  mod_demografi_server('demografi', aktiv = reactive(input$flik == 'Demografi'))

})
