shinyServer(function(input, output, session) {

  # Övriga flikar är platshållare utan server tills vidare.
  mod_oversikt_server('oversikt')
  mod_utbildning_yrken_server('utbildning_yrken',
                              aktiv = reactive(input$flik == 'Utbildning & yrken'))

})
