# =====================================================================
#  mod_geografi.R – flik: Geografi
#
#  Karta över sysselsatta per 1000-metersruta (arbetsställets läge) i
#  Dalarnas län, för en vald bransch (i någon av appens branschindelningar)
#  eller ett valt yrke (SSYK 4-siffrigt). Bara branscher/yrken med minst
#  URVAL_MIN_SYSS sysselsatta i länet går att välja.
#
#  Rutor med färre än RUTA_LITEN_GRANS sysselsatta visas som en egen klass
#  i ljusaste färgen; övriga på en kontinuerlig logaritmisk skala (antalet
#  per ruta spänner från ett fåtal till tusentals).
#  Data hämtas först när fliken öppnas.
# =====================================================================

RUTOR_ALLA <- '__alla__'

mod_geografi_ui <- function(id) {
  ns <- NS(id)

  div(class = 'rd-app',

      div(class = 'rd-sidebar',
          h3('Val'),
          div(class = 'rd-field',
              selectInput(ns('ar_val'), 'År', choices = NULL)),
          div(class = 'rd-field',
              tags$label('Visa efter'),
              shinyWidgets::radioGroupButtons(
                ns('typ'), label = NULL, individual = TRUE, size = 'sm',
                choices = c('Bransch' = 'bransch', 'Yrke' = 'yrke'), selected = 'bransch')),
          conditionalPanel(
            "input.typ == 'bransch'", ns = ns,
            div(class = 'rd-field',
                selectInput(ns('indelning_val'), 'Branschindelning',
                            choices = INDELNING_KOLUMNER, selected = 'grupp_benamning')),
            div(class = 'rd-field',
                selectizeInput(ns('bransch_val'), 'Bransch', choices = NULL,
                               options = list(placeholder = 'Sök bransch…')))),
          conditionalPanel(
            "input.typ == 'yrke'", ns = ns,
            div(class = 'rd-field',
                selectizeInput(ns('yrke_val'), 'Yrke', choices = NULL,
                               options = list(placeholder = 'Sök yrke…')))),
          div(class = 'rd-info',
              tags$strong('Om kartan: '),
              'Varje ruta är 1 × 1 km och visar antal sysselsatta med arbetsställe i ',
              'rutan. Rutor med färre än ', RUTA_LITEN_GRANS, ' sysselsatta visas i den ',
              'ljusaste färgen. Bara branscher och yrken med minst ', URVAL_MIN_SYSS,
              ' sysselsatta i länet går att välja.')
      ),

      div(class = 'rd-main',
          div(class = 'rd-card',
              h2(textOutput(ns('rubrik'), inline = TRUE)),
              div(class = 'rd-subtitle', textOutput(ns('underrubrik'), inline = TRUE)),
              leaflet::leafletOutput(ns('karta'), height = '70vh'),
              div(class = 'rd-subtitle', style = 'margin-top: 8px;',
                  paste0('Källa: ', KALLA_RUTOR, '.')))
      )
  )
}

mod_geografi_server <- function(id, aktiv = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {

    forsta_oppning <- shiny::reactive(if (isTRUE(aktiv())) TRUE else NULL)
    oppnad <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(forsta_oppning(), once = TRUE, oppnad(TRUE))

    ar_lista <- shiny::reactive({ shiny::req(oppnad()); hamta_ar_lista_rutor() })
    shiny::observeEvent(ar_lista(), once = TRUE, {
      updateSelectInput(session, 'ar_val', choices = ar_lista(), selected = ar_lista()[1])
    })

    # ---- Valbara branscher/yrken (minst URVAL_MIN_SYSS i länet) -----------

    valbara_branscher <- shiny::reactive({
      shiny::req(input$ar_val, input$indelning_val)
      rutor_valbara_branscher(input$ar_val, input$indelning_val)
    })
    shiny::observeEvent(valbara_branscher(), {
      val <- c('Alla branscher' = RUTOR_ALLA,
               stats::setNames(valbara_branscher()$grupp_namn, valbara_branscher()$grupp_namn))
      behall <- shiny::isolate(input$bransch_val)
      updateSelectizeInput(session, 'bransch_val', choices = val,
                           selected = if (isTRUE(behall %in% val)) behall else RUTOR_ALLA)
    })

    valbara_yrken <- shiny::reactive({
      shiny::req(input$ar_val, input$typ == 'yrke')
      rutor_valbara_yrken(input$ar_val)
    })
    shiny::observeEvent(valbara_yrken(), {
      val <- c('Alla yrken' = RUTOR_ALLA,
               stats::setNames(valbara_yrken()$yrkeskod, valbara_yrken()$yrke))
      behall <- shiny::isolate(input$yrke_val)
      updateSelectizeInput(session, 'yrke_val', choices = val,
                           selected = if (isTRUE(behall %in% val)) behall else RUTOR_ALLA)
    })

    # ---- Urval -> rutor -----------------------------------------------------

    urval <- shiny::reactive({
      shiny::req(input$ar_val, input$typ)
      if (input$typ == 'bransch') {
        shiny::req(input$bransch_val)
        if (input$bransch_val == RUTOR_ALLA) return(list(namn = 'Alla branscher'))
        rad <- dplyr::filter(valbara_branscher(), grupp_namn == input$bransch_val)
        shiny::req(nrow(rad) == 1)
        list(namn = rad$grupp_namn, sni2 = rad$sni2[[1]])
      } else {
        shiny::req(input$yrke_val)
        if (input$yrke_val == RUTOR_ALLA) return(list(namn = 'Alla yrken'))
        rad <- dplyr::filter(valbara_yrken(), yrkeskod == input$yrke_val)
        shiny::req(nrow(rad) == 1)
        list(namn = rad$yrke, yrkeskoder = rad$yrkeskod)
      }
    })

    rutor <- shiny::reactive({
      u <- urval()
      hamta_rutor_km(input$ar_val, sni2 = u$sni2, yrkeskoder = u$yrkeskoder) |>
        rutor_till_wgs84()
    })

    output$rubrik <- renderText({
      paste0('Sysselsatta per km-ruta: ', urval()$namn, ', ', input$ar_val)
    })
    output$underrubrik <- renderText({
      d <- rutor()
      paste0(.rutor_tal(sum(d$antal)), ' sysselsatta med arbetsställe i ',
             .rutor_tal(nrow(d)), ' rutor i Dalarnas län. ',
             'Håll muspekaren över en ruta för antal.')
    })

    # ---- Karta --------------------------------------------------------------

    output$karta <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE, zoomSnap = 0.25)) |>
        leaflet::addProviderTiles('OpenStreetMap.Mapnik',
                                  options = leaflet::providerTileOptions(opacity = 0.45)) |>
        leaflet::fitBounds(12.1, 59.8, 16.8, 62.3)
    })

    # Proxy-anrop innan kartan finns i webbläsaren går förlorade, så rita
    # först när kartan rapporterat sina gränser en gång.
    karta_klar <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$karta_bounds, once = TRUE, karta_klar(TRUE))

    shiny::observe({
      shiny::req(karta_klar())
      d <- rutor()
      proxy <- leaflet::leafletProxy('karta') |>
        leaflet::clearShapes() |>
        leaflet::clearControls()
      if (nrow(d) == 0) return(invisible(NULL))

      liten <- d$antal < RUTA_LITEN_GRANS
      pal <- leaflet::colorNumeric(
        RUTOR_SKALA, domain = log10(c(RUTA_LITEN_GRANS, max(d$antal, RUTA_LITEN_GRANS + 1))))
      fyllning <- ifelse(liten, RUTA_LITEN_FARG, pal(log10(pmax(d$antal, RUTA_LITEN_GRANS))))
      etiketter <- lapply(paste0(
        '1 × 1 km-ruta<br><b>',
        ifelse(liten, paste0('1–', RUTA_LITEN_GRANS - 1), .rutor_tal(d$antal)),
        '</b> sysselsatta'), htmltools::HTML)

      brytpunkter <- c(4, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000)
      brytpunkter <- brytpunkter[brytpunkter >= RUTA_LITEN_GRANS & brytpunkter <= max(d$antal)]

      proxy |>
        leaflet::addRectangles(
          data = d, lng1 = ~lng1, lat1 = ~lat1, lng2 = ~lng2, lat2 = ~lat2,
          fillColor = fyllning, fillOpacity = 0.9, stroke = FALSE,
          label = etiketter,
          highlightOptions = leaflet::highlightOptions(stroke = TRUE, weight = 2,
                                                       color = RD_TEXT, opacity = 1)) |>
        leaflet::addLegend(
          'bottomright', title = 'Sysselsatta per ruta', opacity = 0.85,
          colors = c(RUTA_LITEN_FARG, pal(log10(brytpunkter))),
          labels = c(paste0('1–', RUTA_LITEN_GRANS - 1), .rutor_tal(brytpunkter)))
    })
  })
}

.rutor_tal <- function(x) format(round(x), big.mark = ' ', scientific = FALSE, trim = TRUE)
