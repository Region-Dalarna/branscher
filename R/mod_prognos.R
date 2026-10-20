# =====================================================================
#  mod_prognos.R – flik: Prognos (SCB:s RegTop2025)
#
#  Innehåll:
#   - Val av prognosår (förval 2040) och näringsgrenar för
#     utvecklingsdiagrammet
#   - Nyckeltal för länet: förvärvsarbetande basåret och prognosåret,
#     förändring (med riket som jämförelse) och arbetskraftens förändring
#   - Förändring per näringsgren, länet och riket
#   - Utveckling år för år (index eller antal)
#   - Arbetskraften per ålder och kön, basåret och prognosåret
#   - SCB:s fotnoter
#
#  RegTop finns bara för län och riket, och använder egna 24
#  näringsgrenar (inte SNI) -- fliken har därför egna val och följer
#  inte övriga flikars geografi och branschindelning.
#  Data hämtas först när fliken öppnas.
# =====================================================================

mod_prognos_ui <- function(id) {
  ns <- NS(id)

  div(class = 'rd-app',

      div(class = 'rd-sidebar',
          h3('Val'),
          div(class = 'rd-field',
              selectInput(ns('ar_val'), 'Prognosår', choices = NULL)),
          div(class = 'rd-field',
              selectizeInput(ns('naringsgren_val'), 'Näringsgrenar i utvecklingsdiagrammet',
                             choices = NULL, multiple = TRUE,
                             options = list(plugins = list('remove_button'), maxItems = 6,
                                            placeholder = 'Välj näringsgrenar…'))),
          div(class = 'rd-info',
              tags$strong('Om prognosen: '),
              'SCB:s Regionala trender och prognoser 2025. Finns bara för ',
              'län och riket, och med SCB:s egna 24 näringsgrenar – därför har ',
              'fliken egna val och följer inte geografin och branschindelningen i ',
              'övriga flikar.')
      ),

      div(class = 'rd-main',

          div(class = 'rd-kpi-row rd-kpi-row--4',
              rd_kpi(textOutput(ns('etikett_bas'), inline = TRUE), textOutput(ns('box_bas')),
                     paste('Antal förvärvsarbetande med arbetsställe i Dalarnas län',
                           '(dagbefolkning, 16–74 år), sista historiska året.')),
              rd_kpi(textOutput(ns('etikett_ar'), inline = TRUE), textOutput(ns('box_ar')),
                     paste('SCB:s prognos för antalet förvärvsarbetande i Dalarnas län',
                           'valt prognosår.')),
              rd_kpi('Förändring', textOutput(ns('box_forandring')),
                     paste('Procentuell förändring av antalet förvärvsarbetande i',
                           'länet fram till valt prognosår, med riket som jämförelse.'),
                     under = textOutput(ns('riket_forandring'), inline = TRUE)),
              rd_kpi('Arbetskraften', textOutput(ns('box_arbetskraft')),
                     paste('Procentuell förändring av arbetskraften i länet',
                           '(förvärvsarbetande och arbetslösa, nattbefolkning, 16–74 år)',
                           'fram till valt prognosår.'),
                     under = textOutput(ns('riket_arbetskraft'), inline = TRUE))
          ),

          div(class = 'rd-card',
              h2(textOutput(ns('rubrik_forandring'), inline = TRUE)),
              div(class = 'rd-subtitle',
                  'Procentuell förändring av antalet förvärvsarbetande per näringsgren, ',
                  'Dalarnas län och riket. Håll muspekaren över en punkt för antal.'),
              girafeOutput(ns('plot_forandring'), height = 'auto')),

          div(class = 'rd-card',
              h2('Utveckling år för år'),
              div(class = 'rd-subtitle',
                  'Förvärvsarbetande i de näringsgrenar som är valda i sidopanelen. ',
                  'Index visar länet (heldraget) och riket (streckat) på samma skala.'),
              div(class = 'rd-sortering',
                  span(class = 'rd-sortering__label', 'Visa:'),
                  shinyWidgets::radioGroupButtons(
                    ns('visa_utveckling'), label = NULL, individual = TRUE, size = 'sm',
                    choices = c('Index, län och riket' = 'index', 'Antal, länet' = 'antal'),
                    selected = 'index')),
              girafeOutput(ns('plot_utveckling'), height = 'auto')),

          div(class = 'rd-card',
              h2(textOutput(ns('rubrik_arbetskraft'), inline = TRUE)),
              div(class = 'rd-subtitle', 'Arbetskraften i Dalarnas län per ålder och kön.'),
              uiOutput(ns('arbetskraft_legend')),
              girafeOutput(ns('plot_arbetskraft'), height = 'auto')),

          tags$details(class = 'rd-card rd-fotnoter',
                       tags$summary('SCB:s fotnoter till prognosen'),
                       uiOutput(ns('fotnoter')))
      )
  )
}

mod_prognos_server <- function(id, aktiv = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {

    # ---- Data (hämtas när fliken öppnas första gången) -------------------

    forsta_oppning <- shiny::reactive(if (isTRUE(aktiv())) TRUE else NULL)
    oppnad <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(forsta_oppning(), once = TRUE, oppnad(TRUE))

    naring <- shiny::reactive({ shiny::req(oppnad()); hamta_naringsgren_prognos() })
    arbetskraft <- shiny::reactive({ shiny::req(oppnad()); hamta_arbetskraft_prognos() })
    basar <- shiny::reactive(regtop_basar())

    shiny::observeEvent(naring(), once = TRUE, {
      prognosar <- regtop_prognosar()
      updateSelectInput(session, 'ar_val', choices = rev(prognosar), selected = max(prognosar))

      grenar <- naring() |>
        dplyr::filter(regionkod == REGTOP_LAN, ar == basar(),
                      naringsgren_kod != REGTOP_NARINGSGREN_SAKNAS) |>
        dplyr::arrange(naringsgren_kod)
      updateSelectizeInput(session, 'naringsgren_val',
                           choices  = stats::setNames(grenar$naringsgren_kod, grenar$naringsgren),
                           selected = storsta_naringsgrenar(naring(), basar(), n = 3))
    })

    ar_val <- shiny::reactive({ shiny::req(input$ar_val); as.integer(input$ar_val) })
    period <- shiny::reactive(paste0(basar(), '–', ar_val()))

    # ---- Nyckeltal -------------------------------------------------------

    total <- function(d, reg, ar) regtop_total(d, reg, ar)
    forandring <- function(d, reg) total(d, reg, ar_val()) / total(d, reg, basar()) - 1

    output$etikett_bas <- renderText(paste('Förvärvsarbetande', basar()))
    output$etikett_ar  <- renderText(paste('Förvärvsarbetande', ar_val()))
    output$box_bas     <- renderText(.tal(total(naring(), REGTOP_LAN, basar())))
    output$box_ar      <- renderText(.tal(total(naring(), REGTOP_LAN, ar_val())))
    output$box_forandring   <- renderText(.pct(forandring(naring(), REGTOP_LAN)))
    output$riket_forandring <- renderText(paste('Riket:', .pct(forandring(naring(), '00'))))
    output$box_arbetskraft   <- renderText(.pct(forandring(arbetskraft(), REGTOP_LAN)))
    output$riket_arbetskraft <- renderText(paste('Riket:', .pct(forandring(arbetskraft(), '00'))))

    # ---- Diagram ---------------------------------------------------------

    output$rubrik_forandring  <- renderText(paste('Förändring per näringsgren', period()))
    output$rubrik_arbetskraft <- renderText(paste('Arbetskraften', basar(), 'och', ar_val()))

    output$plot_forandring <- renderGirafe(
      skapa_diagram_naringsgren_forandring(
        naringsgren_forandring(naring(), basar(), ar_val()), basar(), ar_val(),
        underrubrik = paste('Dalarnas län och riket ·', period()),
        kalla = REGTOP_KALLA)
    )

    output$plot_utveckling <- renderGirafe(
      skapa_diagram_naringsgren_utveckling(
        naring(), input$naringsgren_val, basar(), input$visa_utveckling %||% 'index',
        underrubrik = paste0(basar(), ' historik, ', basar() + 1, '–', max(naring()$ar), ' prognos'),
        kalla = REGTOP_KALLA)
    )

    # Teckenförklaring för fylld stapel / ram -- se skapa_diagram_arbetskraft_pyramid()
    # i func_diagram_regtop.R för vilken data som faktiskt ritas som vad.
    output$arbetskraft_legend <- renderUI({
      shiny::req(basar(), ar_val())
      div(class = 'rd-figurlegend',
          div(class = 'rd-figurlegend__item',
              span(class = 'rd-figurlegend__swatch rd-figurlegend__swatch--fylld'),
              paste('Sista historiska året,', basar())),
          div(class = 'rd-figurlegend__item',
              span(class = 'rd-figurlegend__swatch rd-figurlegend__swatch--ram'),
              paste('Valt prognosår,', ar_val())))
    })

    output$plot_arbetskraft <- renderGirafe(
      skapa_diagram_arbetskraft_pyramid(
        arbetskraft(), basar(), ar_val(),
        underrubrik = 'Dalarnas län',
        kalla = REGTOP_KALLA)
    )

    output$fotnoter <- renderUI({
      shiny::req(oppnad())
      tabeller <- c('naringsgren_prognos' = 'Förvärvsarbetande per näringsgren',
                    'arbetskraft_kon_alder' = 'Arbetskraften per kön och ålder')
      tagList(lapply(names(tabeller), function(t) {
        txt <- hamta_regtop_fotnot(t)
        tagList(tags$h4(tabeller[[t]]),
                tags$p(class = 'rd-fotnot',
                       if (is.na(txt) || !nzchar(txt)) 'Inga fotnoter hittades.' else txt))
      }))
    })
  })
}
