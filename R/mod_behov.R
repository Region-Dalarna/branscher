# =====================================================================
#  mod_behov.R – flik: Behov & rekrytering (SCB:s RegTop2025)
#
#  SCB:s bedömning av arbetsmarknadsläget 2040 per utbildningsgrupp i
#  Dalarnas län, sedd från branschens och yrkenas håll.
#   - Gemensamma val (mod_urval_yrke): år, geografi, branschindelning,
#     bransch
#   - Underflik Branscher (förval):
#       nyckeltal: andel av de sysselsatta med brist- resp.
#       överskottsutbildning, andel som ingår i bedömningen
#       vilka utbildningar har de sysselsatta i branschen (färgade efter
#       bedömning), branscher mest beroende av bristutbildningar
#     Utbildningsstrukturen kommer från mikrodatan (utb_yrken_branscher,
#     sun2020grp_t23) och kopplas till RegTop:s utbildningsgrupper via
#     regtop.nyckel_utbildningsgrupper.
#   - Underflik Yrken: yrken mest beroende av bristutbildningar (RegTop:s
#     yrkesstruktur 2023, Dalarnas län). Yrket som är valt i Utbildning &
#     yrken framhävs; ett klick på ett yrke framhäver det.
#  Bedömningen gäller alltid Dalarnas län, även om en kommun är vald.
#  Data hämtas när fliken öppnas.
# =====================================================================

mod_behov_ui <- function(id) {
  ns <- NS(id)

  div(class = 'rd-app',

      mod_urval_yrke_ui(
        ns('urval'),
        div(class = 'rd-info',
            tags$strong('Om bedömningen: '),
            'SCB:s Regionala trender och prognoser 2025 (RegTop) bedömer ',
            'arbetsmarknadsläget 2040 för utbildningsgrupper i Dalarnas län – ',
            'från stor brist till stort överskott. Bedömningen gäller länet även ',
            'när en kommun är vald.')
      ),

      div(class = 'rd-main',
          tabsetPanel(
            id = ns('underflik'),

            tabPanel('Branscher',
                     div(class = 'rd-kpi-row rd-underflik-kpi',
                         rd_kpi('Med bristutbildning', textOutput(ns('box_brist')),
                                paste('Andel av de sysselsatta i urvalet vars utbildning SCB bedömer',
                                      'ger stor brist, brist eller viss brist i Dalarnas län 2040.',
                                      'En hög andel tyder på att det kan bli svårt att rekrytera',
                                      'när de går i pension eller slutar.'),
                                under = textOutput(ns('urval_txt'), inline = TRUE)),
                         rd_kpi('Med överskottsutbildning', textOutput(ns('box_overskott')),
                                paste('Andel av de sysselsatta i urvalet vars utbildning SCB bedömer',
                                      'ger visst överskott, överskott eller stort överskott 2040.'),
                                under = textOutput(ns('urval_txt2'), inline = TRUE)),
                         rd_kpi('Ingår i bedömningen', textOutput(ns('box_ingar')),
                                paste('Andel av de sysselsatta som har en utbildning som SCB bedömer.',
                                      'Bedömningen omfattar inte alla utbildningar, till exempel inte',
                                      'grundskola eller eftergymnasiala studier utan examen.'),
                                under = textOutput(ns('urval_txt3'), inline = TRUE))),

                     div(class = 'rd-card',
                         h2(textOutput(ns('rubrik_utbildningar'), inline = TRUE)),
                         div(class = 'rd-subtitle',
                             'Andel av de sysselsatta per utbildning, färgad efter SCB:s bedömning ',
                             'av arbetsmarknadsläget för utbildningen i Dalarnas län 2040.'),
                         girafeOutput(ns('plot_utbildningar'), height = 'auto')),

                     div(class = 'rd-card',
                         h2('Branscher mest beroende av bristutbildningar'),
                         div(class = 'rd-subtitle',
                             'Alla branscher i vald branschindelning, sorterade efter andelen sysselsatta ',
                             'med en utbildning som väntas ge brist 2040. Vald bransch framhävs.'),
                         girafeOutput(ns('plot_branscher'), height = 'auto'))),

            tabPanel('Yrken',
                     div(class = 'rd-card rd-underflik-kort',
                         h2('Yrken mest beroende av bristutbildningar'),
                         div(class = 'rd-subtitle',
                             'De 20 yrken där störst andel av de förvärvsarbetande har en utbildning ',
                             'som väntas ge brist 2040 (SCB:s yrkesgrupper, Dalarnas län, yrken med minst ',
                             BEHOV_MIN_YRKE, ' förvärvsarbetande). Yrket som är valt i Utbildning & yrken ',
                             'framhävs; klicka på ett yrke för att framhäva det.'),
                         girafeOutput(ns('plot_yrken'), height = 'auto')))
          ),

          tags$details(class = 'rd-card rd-fotnoter',
                       tags$summary('SCB:s fotnoter'),
                       uiOutput(ns('fotnoter')))
      )
  )
}

# extern_yrke: reaktiv med yrke (SSYK 2012 tresiffrig) valt i Utbildning &
# yrken; motsvarande RegTop-yrkesgrupp framhävs under Yrken.
mod_behov_server <- function(id, aktiv = shiny::reactive(TRUE),
                             extern_yrke = shiny::reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    urval <- mod_urval_yrke_server('urval', aktiv)

    # ---- Branscher -------------------------------------------------------

    # Alla branscher i vald indelning, vald geografi och år.
    bub <- shiny::reactive(
      hamta_bransch_utbildning_bedomning(urval$ar(), urval$geografi(), urval$indelning())
    ) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$indelning(), 'behov')

    valda <- shiny::reactive(if (nzchar(urval$bransch())) urval$bransch() else NULL)
    utbildningar <- shiny::reactive(bransch_utbildningar(bub(), valda()))

    andel <- function(nivaer) {
      d <- utbildningar()
      if (sum(d$antal) < MIN_NAMNARE) return(NA_real_)
      sum(d$andel[d$bedomning %in% nivaer])
    }
    pct <- function(x) if (is.na(x)) '–' else .pct(x, plus = FALSE)

    urval_txt <- shiny::reactive(paste0(valda() %||% 'Alla branscher', ' · ', urval$geo_namn()))
    output$urval_txt  <- renderText(urval_txt())
    output$urval_txt2 <- renderText(urval_txt())
    output$urval_txt3 <- renderText(urval_txt())

    output$box_brist     <- renderText(pct(andel(BEDOMNING_BRIST)))
    output$box_overskott <- renderText(pct(andel(BEDOMNING_OVERSKOTT)))
    output$box_ingar     <- renderText(pct(1 - andel(c(BEDOMNING_INGAR_INTE))))

    output$rubrik_utbildningar <- renderText(paste0(
      'Vilka utbildningar har de sysselsatta i ',
      if (is.null(valda())) 'alla branscher' else tolower(valda()), '?'))

    output$plot_utbildningar <- renderGirafe({
      d <- utbildningar()
      if (sum(d$antal) < MIN_NAMNARE) {
        return(.girafe_std(.tom_plot(paste0('Färre än ', MIN_NAMNARE,
                                            ' sysselsatta i urvalet – andelar visas inte')),
                           height_svg = 1.5))
      }
      skapa_diagram_utbildningar(
        d, andel_txt = 'Andel av de sysselsatta',
        underrubrik = paste0(urval$underrubrik(), ' · bedömning 2040 för Dalarnas län'),
        kalla = paste0(KALLA_YRKE_UTB, '; ', REGTOP_KALLA))
    })

    output$plot_branscher <- renderGirafe({
      d <- bransch_andel_per_bedomning(bub()) |>
        dplyr::filter(antal >= MIN_NAMNARE, bedomning %in% BEDOMNING_BRIST)
      skapa_diagram_bristberoende(
        d, markerad = valda(), topp_n = Inf, antal_txt = 'sysselsatta', klickbar = FALSE,
        underrubrik = paste0(urval$geo_namn(), ' · år ', urval$ar(),
                             ' · bedömning 2040 för Dalarnas län'),
        kalla = paste0(KALLA_YRKE_UTB, '; ', REGTOP_KALLA))
    })

    # ---- Yrken (RegTop:s yrkesstruktur, Dalarnas län) -------------------

    forsta_oppning <- shiny::reactive(if (isTRUE(aktiv())) TRUE else NULL)
    oppnad <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(forsta_oppning(), once = TRUE, oppnad(TRUE))

    yrken <- shiny::reactive({ shiny::req(oppnad()); hamta_yrken_lan() })
    per_bedomning_yrke <- shiny::reactive({
      shiny::req(oppnad())
      hamta_yrke_utbildning_bedomning() |>
        dplyr::filter(yrke_kod %in% yrken()$yrke_kod) |>
        yrke_andel_per_bedomning()
    })

    # Framhävt yrke: följer Utbildning & yrken, eller ett klick i diagrammet
    # (det senaste som hänt gäller).
    klickat <- shiny::reactiveVal(NULL)
    shiny::observeEvent(extern_yrke(), klickat(NULL), ignoreInit = TRUE)
    shiny::observeEvent(input$plot_yrken_selected, {
      val <- input$plot_yrken_selected
      if (length(val) == 1 && nzchar(val)) klickat(val)
    })
    markerat_yrke <- shiny::reactive(klickat() %||% regtop_yrke_for_ssyk3(extern_yrke(), yrken()))

    output$plot_yrken <- renderGirafe({
      stora <- yrken() |>
        dplyr::filter(antal >= BEHOV_MIN_YRKE | yrke_kod %in% markerat_yrke())
      d <- per_bedomning_yrke() |>
        dplyr::filter(bedomning %in% BEDOMNING_BRIST) |>
        dplyr::inner_join(dplyr::select(stora, enhet_kod = yrke_kod, antal), by = 'enhet_kod')
      skapa_diagram_bristberoende(
        d, markerad = markerat_yrke(),
        underrubrik = 'Dalarnas län · yrkesstruktur 2023, bedömning 2040',
        kalla = REGTOP_KALLA)
    })

    # ---- Fotnoter --------------------------------------------------------

    output$fotnoter <- renderUI({
      shiny::req(oppnad())
      tabeller <- c('bedomning_2040' = 'Bedömning av arbetsmarknadsläget 2040',
                    'yrke_utbildning_andel' = 'Utbildningsstruktur per yrke',
                    'yrkesstruktur' = 'Förvärvsarbetande per yrkesgrupp',
                    'nyckel_utbildningsgrupper' = 'Koppling mellan utbildningsgrupper')
      tagList(lapply(names(tabeller), function(t) {
        txt <- hamta_regtop_fotnot(t)
        tagList(tags$h4(tabeller[[t]]),
                tags$p(class = 'rd-fotnot',
                       if (is.na(txt) || !nzchar(txt)) 'Inga fotnoter hittades.' else txt))
      }))
    })
  })
}
