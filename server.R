shinyServer(function(input, output, session) {

  telemetri_server(telemetry, navigation_id = 'flikval', forsta_flik = 'Tab 1')

  output$example_text <- renderText({{
    'Byt ut detta mot din egen serverlogik.'
  }})

})
