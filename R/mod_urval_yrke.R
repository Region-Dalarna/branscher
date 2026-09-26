# =====================================================================
#  mod_urval_yrke.R – gemensamma val för flikarna som bygger på
#  mikro_db.utb_yrken_branscher (Utbildning & yrken, Matchning,
#  Demografi): år, geografi, branschindelning och bransch.
#
#  mod_urval_yrke_ui() ger sidopanelens innehåll; fältspecifika val
#  (t.ex. utbildningsindelning) skickas in via `...` och hamnar före
#  sekretessrutan.
#
#  mod_urval_yrke_server() fyller valen först när fliken öppnas första
#  gången (aktiv() blir TRUE) och returnerar en lista med reaktiva
#  värden. Alla datafrågor i flikarna väntar (req) på dessa, så ingen
#  fråga går mot databasen innan fliken visats.
# =====================================================================

mod_urval_yrke_ui <- function(id, ...) {
  ns <- NS(id)

  div(class = 'rd-sidebar',
      h3('Val'),
      div(class = 'rd-field', selectInput(ns('ar_val'), 'År', choices = NULL)),
      div(class = 'rd-field', selectInput(ns('geografi_val'), 'Geografi', choices = NULL)),
      div(class = 'rd-field', selectInput(ns('indelning_val'), 'Branschindelning', choices = NULL)),
      div(class = 'rd-field', selectInput(ns('bransch_val'), 'Bransch',
                                          choices = c('Alla branscher' = ''))),
      ...,
      div(class = 'rd-info',
          tags$strong('OBS: '),
          'Antal under 4 visas som “färre än 4”. Andelar visas bara ',
          'när underlaget är minst ', MIN_NAMNARE, ' sysselsatta.')
  )
}

mod_urval_yrke_server <- function(id, aktiv = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {

    kommuner <- hamta_kommuner()
    geo_val  <- c('Hela Dalarna (länet)' = '20', 'Riket' = '00',
                  stats::setNames(kommuner$kommun_kod, kommuner$kommun_namn))

    forsta_oppning <- shiny::reactive(if (isTRUE(aktiv())) TRUE else NULL)

    shiny::observeEvent(forsta_oppning(), once = TRUE, {
      ar_lista    <- hamta_ar_lista_yrke_utb()
      indelningar <- hamta_indelningar()
      updateSelectInput(session, 'ar_val', choices = ar_lista, selected = ar_lista[1])
      updateSelectInput(session, 'geografi_val', choices = geo_val)
      updateSelectInput(session, 'indelning_val',
                        choices  = stats::setNames(indelningar$kolumn, indelningar$namn),
                        selected = 'grupp_benamning')
    })

    shiny::observeEvent(input$indelning_val, {
      shiny::req(input$indelning_val)  # "" vid start, innan valen fyllts
      grp <- hamta_grupper_for_indelning(input$indelning_val)
      updateSelectInput(session, 'bransch_val',
                        choices = c('Alla branscher' = '',
                                    stats::setNames(grp$grupp_namn, grp$grupp_namn)))
    })

    ar       <- shiny::reactive({ shiny::req(input$ar_val); input$ar_val })
    geografi <- shiny::reactive({ shiny::req(input$geografi_val); input$geografi_val })
    indelning <- shiny::reactive({ shiny::req(input$indelning_val); input$indelning_val })
    bransch  <- shiny::reactive(input$bransch_val %||% '')

    branschkoder <- shiny::reactive(hamta_branschkoder(indelning(), bransch()))

    geo_namn <- shiny::reactive({
      namn <- names(geo_val)[geo_val == geografi()]
      if (geografi() == '20') 'Dalarna' else namn
    })

    underrubrik <- shiny::reactive(paste0(
      geo_namn(), ' · ',
      if (nzchar(bransch())) bransch() else 'Alla branscher',
      ' · år ', ar()
    ))

    list(ar = ar, geografi = geografi, indelning = indelning, bransch = bransch,
         branschkoder = branschkoder, geo_namn = geo_namn, underrubrik = underrubrik)
  })
}
