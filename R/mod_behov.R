# =====================================================================
#  mod_behov.R – flik: Behov & rekrytering (SCB:s RegTop2025)
#
#  SCB:s bedömning av arbetsmarknadsläget 2040 per utbildningsgrupp,
#  sedd från yrkenas håll: vilka utbildningar rekryterar ett yrke från,
#  och hur många av dem väntas bli bristutbildningar?
#   - Val av yrke (RegTop:s yrkesgrupper). Följer yrket som är valt i
#     Utbildning & yrken när det finns en motsvarighet.
#   - Nyckeltal: förvärvsarbetande i yrket, andel med bristutbildning,
#     andel med överskottsutbildning
#   - Vilka utbildningar rekryterar yrket från (färgade efter bedömning)
#   - Yrken mest beroende av bristutbildningar (klick väljer yrke)
#  Dalarnas län, 2023 års utbildningsstruktur. Data hämtas när fliken
#  öppnas. Kartor per län finns i Utbildning-appen.
# =====================================================================

mod_behov_ui <- function(id) {
  ns <- NS(id)

  div(class = 'rd-app',

      div(class = 'rd-sidebar',
          h3('Val'),
          div(class = 'rd-field',
              selectizeInput(ns('yrke_val'), 'Yrke', choices = NULL,
                             options = list(sortField = '$order',
                                            placeholder = 'Sök yrke…')),
              div(class = 'rd-hint',
                  'Yrkena är SCB:s yrkesgrupper i prognosen. Har du valt ett yrke i ',
                  'Utbildning & yrken väljs motsvarande yrke här.')),
          div(class = 'rd-info',
              tags$strong('Om bedömningen: '),
              'SCB:s Regionala trender och prognoser 2025 (RegTop) bedömer ',
              'arbetsmarknadsläget 2040 för varje utbildningsgrupp i Dalarnas län – ',
              'från stor brist till stort överskott. Yrkenas utbildningsstruktur avser 2023.')
      ),

      div(class = 'rd-main',

          div(class = 'rd-kpi-row',
              rd_kpi('Förvärvsarbetande i yrket', textOutput(ns('box_antal')),
                     'Antal förvärvsarbetande i yrket i Dalarnas län 2023 (SCB:s yrkesgrupper i RegTop).'),
              rd_kpi('Med bristutbildning', textOutput(ns('box_brist')),
                     paste('Andel av yrkets förvärvsarbetande vars utbildning SCB bedömer ger',
                           'stor brist, brist eller viss brist i Dalarnas län 2040. En hög andel',
                           'tyder på att yrket kan bli svårt att rekrytera till.')),
              rd_kpi('Med överskottsutbildning', textOutput(ns('box_overskott')),
                     paste('Andel av yrkets förvärvsarbetande vars utbildning SCB bedömer ger',
                           'visst överskott, överskott eller stort överskott i Dalarnas län 2040.'))
          ),

          div(class = 'rd-card',
              h2(textOutput(ns('rubrik_utbildningar'), inline = TRUE)),
              div(class = 'rd-subtitle',
                  'Andel av yrkets förvärvsarbetande per utbildning, färgad efter SCB:s ',
                  'bedömning av arbetsmarknadsläget för utbildningen i Dalarnas län 2040.'),
              girafeOutput(ns('plot_utbildningar'), height = 'auto')),

          div(class = 'rd-card',
              h2('Yrken mest beroende av bristutbildningar'),
              div(class = 'rd-subtitle',
                  'De 20 yrken där störst andel av de förvärvsarbetande har en utbildning ',
                  'som väntas ge brist 2040 (yrken med minst ', BEHOV_MIN_YRKE,
                  ' förvärvsarbetande i länet). Valt yrke framhävs. Klicka på ett yrke för att välja det.'),
              girafeOutput(ns('plot_bristberoende'), height = 'auto')),

          tags$details(class = 'rd-card rd-fotnoter',
                       tags$summary('SCB:s fotnoter'),
                       uiOutput(ns('fotnoter')))
      )
  )
}

# extern_yrke: reaktiv med yrke (SSYK 2012 tresiffrig) valt i en annan
# flik; när fliken öppnas väljs motsvarande RegTop-yrkesgrupp.
mod_behov_server <- function(id, aktiv = shiny::reactive(TRUE),
                             extern_yrke = shiny::reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # ---- Data (hämtas när fliken öppnas första gången) -------------------

    forsta_oppning <- shiny::reactive(if (isTRUE(aktiv())) TRUE else NULL)
    oppnad <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(forsta_oppning(), once = TRUE, oppnad(TRUE))

    yrken <- shiny::reactive({ shiny::req(oppnad()); hamta_yrken_lan() })
    yub   <- shiny::reactive({
      shiny::req(oppnad())
      hamta_yrke_utbildning_bedomning() |> dplyr::filter(yrke_kod %in% yrken()$yrke_kod)
    })
    per_bedomning <- shiny::reactive(yrke_andel_per_bedomning(yub()))

    # ---- Val av yrke -----------------------------------------------------
    # Listan sorteras efter antal förvärvsarbetande (störst först). När
    # fliken öppnas följer valet yrket i Utbildning & yrken om det har en
    # motsvarighet och har ändrats sedan sist; annars behålls valet.

    synkat_externt <- shiny::reactiveVal(NULL)

    shiny::observeEvent(yrken(), once = TRUE, {
      y <- yrken() |> dplyr::filter(yrke_kod %in% yub()$yrke_kod)
      updateSelectizeInput(session, 'yrke_val',
                           choices  = stats::setNames(y$yrke_kod, y$yrke),
                           selected = regtop_yrke_for_ssyk3(extern_yrke(), y) %||% y$yrke_kod[1])
      synkat_externt(extern_yrke())
    })

    shiny::observeEvent(aktiv(), {
      shiny::req(isTRUE(aktiv()), oppnad(), !is.null(input$yrke_val))
      ext <- extern_yrke()
      if (identical(ext, synkat_externt())) return()
      synkat_externt(ext)
      motsv <- regtop_yrke_for_ssyk3(ext, yrken())
      if (!is.null(motsv)) updateSelectizeInput(session, 'yrke_val', selected = motsv)
    })

    # Klick på ett yrke i topplistan väljer det.
    shiny::observeEvent(input$plot_bristberoende_selected, {
      val <- input$plot_bristberoende_selected
      if (length(val) == 1 && nzchar(val)) updateSelectizeInput(session, 'yrke_val', selected = val)
    })

    yrke <- shiny::reactive({ shiny::req(input$yrke_val); input$yrke_val })
    yrke_namn <- shiny::reactive(yrken()$yrke[match(yrke(), yrken()$yrke_kod)])

    # ---- Nyckeltal -------------------------------------------------------

    andel_grupp <- function(grupper) {
      d <- per_bedomning() |> dplyr::filter(yrke_kod == yrke())
      if (nrow(d) == 0) return(NA_real_)
      sum(d$andel[d$bedomning %in% grupper])
    }

    output$box_antal     <- renderText(.tal(yrken()$antal[match(yrke(), yrken()$yrke_kod)]))
    output$box_brist     <- renderText(.pct(andel_grupp(BEDOMNING_BRIST), plus = FALSE))
    output$box_overskott <- renderText(.pct(andel_grupp(BEDOMNING_OVERSKOTT), plus = FALSE))

    # ---- Diagram ---------------------------------------------------------

    output$rubrik_utbildningar <- renderText(
      paste0('Vilka utbildningar rekryterar ', tolower(yrke_namn() %||% 'yrket'), ' från?')
    )

    output$plot_utbildningar <- renderGirafe(
      skapa_diagram_yrke_utbildningar(
        dplyr::filter(yub(), yrke_kod == yrke()),
        underrubrik = paste('Dalarnas län · utbildningsstruktur 2023, bedömning 2040'),
        kalla = REGTOP_KALLA)
    )

    output$plot_bristberoende <- renderGirafe({
      stora <- yrken() |> dplyr::filter(antal >= BEHOV_MIN_YRKE | yrke_kod == yrke())
      d <- per_bedomning() |>
        dplyr::filter(bedomning %in% BEDOMNING_BRIST) |>
        dplyr::inner_join(dplyr::select(stora, yrke_kod, antal), by = "yrke_kod")
      skapa_diagram_bristberoende(
        d, markerad = yrke(),
        underrubrik = 'Dalarnas län · utbildningsstruktur 2023, bedömning 2040',
        kalla = REGTOP_KALLA)
    })

    output$fotnoter <- renderUI({
      shiny::req(oppnad())
      tabeller <- c('bedomning_2040' = 'Bedömning av arbetsmarknadsläget 2040',
                    'yrke_utbildning_andel' = 'Utbildningsstruktur per yrke',
                    'yrkesstruktur' = 'Förvärvsarbetande per yrkesgrupp')
      tagList(lapply(names(tabeller), function(t) {
        txt <- hamta_regtop_fotnot(t)
        tagList(tags$h4(tabeller[[t]]),
                tags$p(class = 'rd-fotnot',
                       if (is.na(txt) || !nzchar(txt)) 'Inga fotnoter hittades.' else txt))
      }))
    })
  })
}
