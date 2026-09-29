# =====================================================================
#  mod_oversikt.R – flik 1: Översikt
#
#  Innehåll:
#   - Val av år, branschindelning, geografisk nivå och bransch (en eller
#     flera, eller Alla branscher)
#   - Tre nyckeltal för valda branscher: sysselsatta, etablerade, andel
#     av rikets sysselsättning (i samma branscher)
#   - Jämförelsediagram: andel sysselsatta per bransch, för vald geografi,
#     länet (Dalarna totalt) och riket samtidigt
#
#  Layout byggd med rd-app/rd-sidebar/rd-main/rd-kpi-row/rd-card, dvs.
#  samma CSS-klasser som redan finns i regiondalarna_ruf.css (sektion
#  4, 5 och 9) -- inte bslib/sidebarLayout.
# =====================================================================

# Värdet för "Alla branscher" i branschväljaren.
ALLA_BRANSCHER <- '__alla__'

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
              selectizeInput(ns('bransch_val'), 'Bransch',
                             choices = c('Alla branscher' = ALLA_BRANSCHER),
                             selected = ALLA_BRANSCHER, multiple = TRUE,
                             options = list(plugins = list('remove_button'),
                                            placeholder = 'V\u00e4lj bransch\u2026'))),

          div(class = 'rd-info',
              tags$strong('OBS: '),
              'V\u00e4rden under 4 visas som \u201cF\u00e4rre \u00e4n 4\u201d av sekretessk\u00e4l.')
      ),

      div(class = 'rd-main',

          div(class = 'rd-kpi-row',
              rd_kpi('Sysselsatta', textOutput(ns('box_sysselsatta')),
                     paste('Antal sysselsatta med arbetsst\u00e4lle i vald geografi',
                           '(dagbefolkning) i valda branscher.'),
                     under = textOutput(ns('urval_txt'), inline = TRUE)),
              rd_kpi('Etablerade', textOutput(ns('box_etablerade')),
                     tagList(
                       'Sysselsatta som \u00e4r etablerade p\u00e5 arbetsmarknaden. Bara anst\u00e4llda ',
                       '(hel\u00e5rsanst\u00e4llda, nyanst\u00e4llda, avg\u00e5ngna och del\u00e5rsanst\u00e4llda) kan ',
                       'r\u00e4knas som etablerade \u2013 egenf\u00f6retagare styr i viss m\u00e5n sin egen ',
                       'inkomst, s\u00e5 f\u00f6r dem g\u00e5r etablering inte att ber\u00e4kna.', tags$br(), tags$br(),
                       'Fr\u00e5n och med 2020 kr\u00e4vs en inkomst p\u00e5 minst 3 inkomstbasbelopp; ',
                       'f\u00f6re 2020 minst 60 % av medianinkomsten f\u00f6r personer med kort ',
                       'f\u00f6rgymnasial utbildning (per \u00e5ldersgrupp och k\u00f6n). I b\u00e5da fallen ',
                       'f\u00e5r personen inte ha haft arbetsl\u00f6shetsers\u00e4ttning under \u00e5ret.'),
                     under = textOutput(ns('urval_txt2'), inline = TRUE)),
              rd_kpi('Andel av rikets syssels\u00e4ttning', textOutput(ns('box_andel')),
                     paste('Sysselsatta med arbetsst\u00e4lle i vald geografi som andel av',
                           'alla sysselsatta i riket inom samma branscher. Med alla',
                           'branscher: andel av hela rikets syssels\u00e4ttning.'),
                     under = textOutput(ns('urval_txt3'), inline = TRUE))
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

    # ---- Branschval: en eller flera, eller Alla branscher -----------------
    # "Alla branscher" och enskilda branscher utesluter varandra: väljs en
    # bransch försvinner "Alla branscher", väljs "Alla branscher" (eller
    # tas alla bort) blir det bara den.

    shiny::observeEvent(grupper_i_indelning(), {
      grp <- grupper_i_indelning()
      behall <- intersect(input$bransch_val, grp$grupp_namn)
      updateSelectizeInput(session, 'bransch_val',
                           choices  = c('Alla branscher' = ALLA_BRANSCHER,
                                        stats::setNames(grp$grupp_namn, grp$grupp_namn)),
                           selected = if (length(behall) > 0) behall else ALLA_BRANSCHER)
    })

    forra_bransch_val <- shiny::reactiveVal(ALLA_BRANSCHER)

    shiny::observeEvent(input$bransch_val, ignoreNULL = FALSE, ignoreInit = TRUE, {
      val <- input$bransch_val
      ny  <- if (length(val) == 0) ALLA_BRANSCHER
             else if (ALLA_BRANSCHER %in% val && length(val) > 1) {
               # "Alla" nyss tillagd -> bara den; annars släpp "Alla".
               if (ALLA_BRANSCHER %in% forra_bransch_val()) setdiff(val, ALLA_BRANSCHER)
               else ALLA_BRANSCHER
             } else val
      forra_bransch_val(ny)
      if (!setequal(ny, val)) updateSelectizeInput(session, 'bransch_val', selected = ny)
    })

    valda_branscher <- shiny::reactive(setdiff(input$bransch_val, ALLA_BRANSCHER))

    branschkoder <- shiny::reactive({
      shiny::req(input$indelning_val)
      hamta_branschkoder(input$indelning_val, valda_branscher())
    })

    urval_txt <- shiny::reactive({
      v <- valda_branscher()
      if (length(v) == 0) 'Alla branscher'
      else if (length(v) <= 2) paste(v, collapse = ' och ')
      else paste(length(v), 'valda branscher')
    })
    output$urval_txt  <- renderText(urval_txt())
    output$urval_txt2 <- renderText(urval_txt())
    output$urval_txt3 <- renderText(urval_txt())

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
      hamta_total_sysselsatta(input$geografi_val, as.integer(input$ar_val), branschkoder()) |>
        formatera_nyckeltal()
    })

    output$box_etablerade <- renderText({
      shiny::req(input$geografi_val, input$ar_val)
      hamta_total_etablerade(input$geografi_val, as.integer(input$ar_val), branschkoder()) |>
        formatera_nyckeltal()
    })

    output$box_andel <- renderText({
      shiny::req(input$geografi_val, input$ar_val)
      ar_int <- as.integer(input$ar_val)
      valt   <- hamta_total_sysselsatta(input$geografi_val, ar_int, branschkoder())
      totalt <- hamta_total_sysselsatta('00', ar_int, branschkoder())  # riket, samma branscher

      if (is.na(valt) || is.na(totalt) || totalt == 0) return('\u2013')
      scales::percent(valt / totalt, accuracy = 0.1, decimal.mark = ',')
    })

    output$plot_jamforelse <- renderGirafe({
      skapa_diagram_bransch_jamforelse(
        df             = data_oversikt(),
        markerad_grupp = valda_branscher(),
        underrubrik    = paste('\u00c5r', input$ar_val),
        kalla          = 'SCB (RAMS), bearbetat av Region Dalarna'
      )
    })
  })
}
