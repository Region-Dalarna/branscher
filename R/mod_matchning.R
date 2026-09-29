# =====================================================================
#  mod_matchning.R – flik: Matchning
#
#  Hur väl de sysselsattas utbildning matchar yrket (kolumnen
#  gruppering i mikro_db.utb_yrken_branscher).
#   - Nyckeltal: andel helt/delvis/inte matchade (bland de tre) och
#     andel anställda utan tillräckliga uppgifter (av anställda)
#   - Underflik Branscher (förvald): matchning per bransch och andel
#     anställda utan tillräckliga uppgifter per bransch. Alla branscher i
#     vald indelning visas; vald bransch framhävs.
#   - Underflik Yrken: matchning per yrke, de 20 största yrkena i urvalet
#     (filtrerat på vald bransch) -- eller de yrken man söker fram och
#     väljer i rutan ovanför diagrammet
# =====================================================================

mod_matchning_ui <- function(id) {
  ns <- NS(id)

  div(class = 'rd-app',
      mod_urval_yrke_ui(ns('urval')),

      div(class = 'rd-main',
          div(class = 'rd-kpi-row rd-kpi-row--4',
              rd_kpi('Helt matchade', textOutput(ns('box_helt')),
                     paste('Andel anst\u00e4llda vars utbildning helt matchar yrket, bland de',
                           'anst\u00e4llda d\u00e4r matchningen kan bed\u00f6mas (helt, delvis eller',
                           'inte matchade).')),
              rd_kpi('Delvis matchade', textOutput(ns('box_delvis')),
                     paste('Andel anst\u00e4llda vars utbildning delvis matchar yrket, bland de',
                           'anst\u00e4llda d\u00e4r matchningen kan bed\u00f6mas.')),
              rd_kpi('Inte matchade', textOutput(ns('box_inte')),
                     paste('Andel anst\u00e4llda vars utbildning inte matchar yrket, bland de',
                           'anst\u00e4llda d\u00e4r matchningen kan bed\u00f6mas.')),
              rd_kpi('Utan tillr\u00e4ckliga uppgifter', textOutput(ns('box_utan')),
                     paste('Andel av alla anst\u00e4llda d\u00e4r yrkes- eller utbildningsuppgift',
                           'saknas, s\u00e5 att matchningen inte kan bed\u00f6mas. En h\u00f6g andel',
                           'g\u00f6r \u00f6vriga andelar os\u00e4krare.'))),

          tabsetPanel(
            id = ns('underflik'),
            tabPanel('Branscher',
                     div(class = 'rd-card',
                         h2('Matchning per bransch'),
                         div(class = 'rd-subtitle',
                             'Alla branscher i vald branschindelning (vald bransch framhävs). ',
                             'Andel helt, delvis och inte matchade bland de anställda där ',
                             'matchningen kan bedömas.'),
                         girafeOutput(ns('plot_matchning_bransch'), height = 'auto')),
                     div(class = 'rd-card',
                         h2('Anställda utan tillräckliga uppgifter per bransch'),
                         div(class = 'rd-subtitle',
                             'Andel av de anställda där yrkes- eller utbildningsuppgift saknas, ',
                             'så att matchning inte kan bedömas. Stapel = vald geografi, ',
                             'grå punkt = riket.'),
                         girafeOutput(ns('plot_utan_uppgifter'), height = 'auto'))),
            tabPanel('Yrken',
                     div(class = 'rd-card',
                         h2('Matchning per yrke'),
                         div(class = 'rd-subtitle',
                             'De 20 största yrkena i urvalet. Yrken du lägger till nedan framhävs ',
                             'och ersätter de minsta \u2013 eller visas ensamma med \u201cVisa bara valda yrken\u201d. ',
                             'Andel helt, delvis och inte matchade bland de anställda där ',
                             'matchningen kan bedömas.'),
                         yrkesval_ui(ns),
                         girafeOutput(ns('plot_matchning_yrke'), height = 'auto')))
          ),

          div(class = 'rd-info',
              tags$strong('Om matchning: '),
              'Helt, delvis och inte matchade redovisas som andel av de anställda där ',
              'matchningen kan bedömas. Egenföretagare och anställda utan ',
              'tillräckliga uppgifter ingår inte i den andelen.')
      )
  )
}

mod_matchning_server <- function(id, aktiv = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {

    urval <- mod_urval_yrke_server('urval', aktiv)

    profil <- shiny::reactive(
      hamta_yrke_profil(urval$ar(), urval$geografi(), urval$branschkoder(), 'gruppering')
    ) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$branschkoder(), 'matchning')

    pct <- function(x) if (is.na(x)) '–' else scales::percent(x, accuracy = 0.1, decimal.mark = ',')
    andel_matchning <- function(grupp) {
      pct(andel_av_total(profil(), 'gruppering', grupp, bland = MATCHNING_GRUPPER))
    }

    output$box_helt   <- renderText(andel_matchning('Helt matchade'))
    output$box_delvis <- renderText(andel_matchning('Delvis matchade'))
    output$box_inte   <- renderText(andel_matchning('Inte matchade'))
    output$box_utan   <- renderText(pct(andel_av_total(
      profil(), 'gruppering', MATCHNING_UTAN, bland = c(MATCHNING_GRUPPER, MATCHNING_UTAN))))

    # Alla branscher i vald indelning (inte filtrerat på vald bransch).
    profil_bransch <- shiny::reactive(
      hamta_bransch_profil(urval$ar(), urval$geografi(), urval$indelning(), 'gruppering')
    ) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$indelning(), 'matchning_bransch')

    matchning_per_enhet <- function(profil, n, markerad, underrubrik, enheter = NULL) {
      d <- profil |>
        dplyr::filter(gruppering %in% MATCHNING_GRUPPER) |>
        fordelning_per_enhet('gruppering', n = n, enheter = enheter)
      skapa_diagram_fordelning(
        d, MATCHNING_GRUPPER, MATCHNING_FARGER,
        markerad         = markerad,
        sortera_kategori = 'Helt matchade',
        underrubrik      = paste0(underrubrik, ' · sorterat efter andel helt matchade'),
        kalla            = KALLA_YRKE_UTB
      )
    }

    # Branschdiagrammen visar alla branscher -- underrubriken nämner
    # därför inte vald bransch.
    output$plot_matchning_bransch <- renderGirafe(
      matchning_per_enhet(profil_bransch(), Inf, urval$bransch(),
                          paste0(urval$geo_namn(), ' · år ', urval$ar()))
    )
    # ---- Egna yrken i Yrken-fliken ---------------------------------------
    # Listan sorteras efter antal sysselsatta i urvalet. Valda yrken som
    # inte finns i ett nytt urval tas bort.

    shiny::observeEvent(profil(), {
      yrken <- profil() |>
        dplyr::filter(!enhet_kod %in% c('***', 'saknas')) |>
        dplyr::count(enhet_kod, enhet_namn, wt = antal, sort = TRUE)
      updateSelectizeInput(session, 'yrken_val',
                           choices  = stats::setNames(yrken$enhet_kod, yrken$enhet_namn),
                           selected = intersect(input$yrken_val, yrken$enhet_kod))
    })

    # Valda yrken läggs till bland de 20 största (de minsta faller bort)
    # och framhävs. Yrken med för litet underlag visas inte -- säg hur många.
    output$plot_matchning_yrke <- renderGirafe({
      valda <- input$yrken_val %||% character(0)
      bara  <- isTRUE(input$bara_valda)
      if (bara && length(valda) == 0) {
        return(.girafe_std(.tom_plot('L\u00e4gg till yrken i s\u00f6krutan ovan'), height_svg = 1.5))
      }
      notis <- notis_for_sma(
        antal_for_sma(dplyr::filter(profil(), gruppering %in% MATCHNING_GRUPPER), valda),
        'anställda med bedömd matchning')
      matchning_per_enhet(profil(), if (bara) 0 else 20, if (bara) NULL else valda,
                          paste0(urval$underrubrik(), notis), enheter = valda)
    })

    utan_uppgifter <- shiny::reactive(
      hamta_andel_utan_uppgifter(urval$ar(), urval$geografi(), urval$indelning())
    ) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$indelning())

    output$plot_utan_uppgifter <- renderGirafe({
      skapa_diagram_andel_inom(
        utan_uppgifter(), geo_namn = urval$geo_namn(),
        markerad    = urval$bransch(),
        underrubrik = paste0(urval$geo_namn(), ' · år ', urval$ar()),
        kalla       = KALLA_YRKE_UTB
      )
    })
  })
}
