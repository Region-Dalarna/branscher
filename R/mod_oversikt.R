# =====================================================================
#  mod_oversikt.R – flik 1: Översikt
#
#  Innehåll:
#   - Val av år, branschindelning, geografisk nivå och markerad bransch
#   - Tre nyckeltal: sysselsatta, etablerade, andel av rikets sysselsättning
#   - Jämförelsediagram: andel sysselsatta per bransch, för vald geografi,
#     länet (Dalarna totalt) och riket samtidigt
#
#  Layout byggd med rd-app/rd-sidebar/rd-main/rd-kpi-row/rd-card, dvs.
#  samma CSS-klasser som redan finns i regiondalarna_ruf.css (sektion
#  4, 5 och 9) -- inte bslib/sidebarLayout.
# =====================================================================

mod_oversikt_ui <- function(id) {
  ns <- NS(id)

  div(class = 'rd-app',

      div(class = 'rd-sidebar',
          h3('Val'),

          div(class = 'rd-field',
              selectInput(ns('ar_val'), '\u00c5r', choices = NULL)),
          div(class = 'rd-field',
              selectInput(ns('indelning_val'), 'Branschindelning', choices = NULL)),
          div(class = 'rd-field',
              selectInput(ns('geografi_val'), 'Geografisk niv\u00e5', choices = NULL)),
          div(class = 'rd-field',
              selectInput(ns('bransch_val'), 'Markera bransch',
                          choices = c('Ingen markering' = ''))),

          div(class = 'rd-info',
              tags$strong('OBS: '),
              'V\u00e4rden under 4 visas som \u201cF\u00e4rre \u00e4n 4\u201d av sekretessk\u00e4l.')
      ),

      div(class = 'rd-main',

          div(class = 'rd-kpi-row',
              div(class = 'rd-kpi',
                  div(class = 'rd-kpi__label', 'Sysselsatta'),
                  div(class = 'rd-kpi__value', textOutput(ns('box_sysselsatta')))),
              div(class = 'rd-kpi',
                  div(class = 'rd-kpi__label', 'Etablerade'),
                  div(class = 'rd-kpi__value', textOutput(ns('box_etablerade')))),
              div(class = 'rd-kpi',
                  div(class = 'rd-kpi__label', 'Andel av rikets syssels\u00e4ttning'),
                  div(class = 'rd-kpi__value', textOutput(ns('box_andel'))))
          ),

          div(class = 'rd-card',
              h2('Andel sysselsatta per bransch'),
              div(class = 'rd-subtitle', 'Vald geografi, l\u00e4net (Dalarna) och riket'),
              girafeOutput(ns('plot_jamforelse'), height = '600px')
          )
      )
  )
}

mod_oversikt_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    indelningar <- hamta_indelningar()
    kommuner    <- hamta_kommuner()
    ar_lista    <- hamta_ar_lista()

    shiny::observe({
      updateSelectInput(session, 'ar_val',
                        choices = ar_lista, selected = ar_lista[1])
    })

    shiny::observe({
      updateSelectInput(session, 'indelning_val',
                        choices  = stats::setNames(indelningar$kolumn, indelningar$namn),
                        selected = 'grupp_benamning')  # bransch_20 saknar värden i källdata -- se func_data.R
    })

    shiny::observe({
      updateSelectInput(session, 'geografi_val',
                        choices = c('Hela Dalarna (l\u00e4net)' = '20',
                                    stats::setNames(kommuner$kommun_kod, kommuner$kommun_namn)))
    })

    grupper_i_indelning <- shiny::reactive({
      shiny::req(input$indelning_val)
      hamta_grupper_for_indelning(input$indelning_val)
    })

    shiny::observeEvent(grupper_i_indelning(), {
      grp <- grupper_i_indelning()
      updateSelectInput(session, 'bransch_val',
                        choices = c('Ingen markering' = '', stats::setNames(grp$grupp_namn, grp$grupp_namn)))
    })

    data_oversikt <- shiny::reactive({
      shiny::req(input$indelning_val, input$geografi_val, input$ar_val)
      hamta_oversiktsdata(
        indelning_kolumn = input$indelning_val,
        geografi          = input$geografi_val,
        ar_val            = as.integer(input$ar_val)
      )
    })

    output$box_sysselsatta <- renderText({
      shiny::req(input$geografi_val, input$ar_val)
      hamta_total_sysselsatta(input$geografi_val, as.integer(input$ar_val)) |>
        formatera_nyckeltal()
    })

    output$box_etablerade <- renderText({
      shiny::req(input$geografi_val, input$ar_val)
      hamta_total_etablerade(input$geografi_val, as.integer(input$ar_val)) |>
        formatera_nyckeltal()
    })

    output$box_andel <- renderText({
      shiny::req(input$geografi_val, input$ar_val)
      ar_int <- as.integer(input$ar_val)
      valt   <- hamta_total_sysselsatta(input$geografi_val, ar_int)
      totalt <- hamta_total_sysselsatta('00', ar_int)  # riket

      if (is.na(valt) || is.na(totalt) || totalt == 0) return('\u2013')
      scales::percent(valt / totalt, accuracy = 0.1)
    })

    output$plot_jamforelse <- renderGirafe({
      skapa_diagram_bransch_jamforelse(
        df             = data_oversikt(),
        markerad_grupp = input$bransch_val,
        underrubrik    = paste('\u00c5r', input$ar_val),
        kalla          = 'SCB (RAMS), bearbetat av Region Dalarna'
      )
    })
  })
}
