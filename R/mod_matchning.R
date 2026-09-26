# =====================================================================
#  mod_matchning.R – flik: Matchning
#
#  Hur väl de sysselsattas utbildning matchar yrket (kolumnen
#  gruppering i mikro_db.utb_yrken_branscher).
#   - Nyckeltal: andel helt/delvis/inte matchade (bland de tre) och
#     andel anställda utan tillräckliga uppgifter (av anställda)
#   - Matchning per yrke, de 20 största yrkena i urvalet
#   - Andel anställda utan tillräckliga uppgifter per bransch
# =====================================================================

mod_matchning_ui <- function(id) {
  ns <- NS(id)

  kpi <- function(etikett, output_id) {
    div(class = 'rd-kpi',
        div(class = 'rd-kpi__label', etikett),
        div(class = 'rd-kpi__value', textOutput(ns(output_id))))
  }

  div(class = 'rd-app',
      mod_urval_yrke_ui(ns('urval')),

      div(class = 'rd-main',
          div(class = 'rd-kpi-row rd-kpi-row--4',
              kpi('Helt matchade', 'box_helt'),
              kpi('Delvis matchade', 'box_delvis'),
              kpi('Inte matchade', 'box_inte'),
              kpi('Utan tillräckliga uppgifter', 'box_utan')),

          div(class = 'rd-card',
              h2('Matchning per yrke'),
              div(class = 'rd-subtitle',
                  'De 20 största yrkena i urvalet. Andel helt, delvis och inte matchade ',
                  'bland de anställda där matchningen kan bedömas.'),
              girafeOutput(ns('plot_matchning'), height = 'auto')),

          div(class = 'rd-card',
              h2('Anställda utan tillräckliga uppgifter per bransch'),
              div(class = 'rd-subtitle',
                  'Andel av de anställda där yrkes- eller utbildningsuppgift saknas, ',
                  'så att matchning inte kan bedömas. Stapel = vald geografi, ',
                  'grå punkt = riket.'),
              girafeOutput(ns('plot_utan_uppgifter'), height = 'auto')),

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

    output$plot_matchning <- renderGirafe({
      d <- profil() |>
        dplyr::filter(gruppering %in% MATCHNING_GRUPPER) |>
        fordelning_per_yrke('gruppering')
      skapa_diagram_fordelning_per_yrke(
        d, MATCHNING_GRUPPER, MATCHNING_FARGER,
        sortera_kategori = 'Helt matchade',
        underrubrik      = paste0(urval$underrubrik(), ' · sorterat efter andel helt matchade'),
        kalla            = KALLA_YRKE_UTB
      )
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
