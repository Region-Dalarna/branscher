# =====================================================================
#  mod_demografi.R – flik: Demografi
#
#  Ålder, kön och bakgrund bland de sysselsatta, per yrke
#  (mikro_db.utb_yrken_branscher).
#   - Nyckeltal: andel i äldsta åldersgruppen, andel kvinnor, andel
#     utrikes födda
#   - Ålder, kön och bakgrund per yrke, de 20 största yrkena i urvalet
# =====================================================================

mod_demografi_ui <- function(id) {
  ns <- NS(id)

  kpi <- function(etikett, output_id) {
    div(class = 'rd-kpi',
        div(class = 'rd-kpi__label', etikett),
        div(class = 'rd-kpi__value', textOutput(ns(output_id))))
  }
  kort <- function(rubrik, underrubrik, output_id) {
    div(class = 'rd-card',
        h2(rubrik),
        div(class = 'rd-subtitle', underrubrik),
        girafeOutput(ns(output_id), height = 'auto'))
  }

  div(class = 'rd-app',
      mod_urval_yrke_ui(ns('urval')),

      div(class = 'rd-main',
          div(class = 'rd-kpi-row',
              kpi(textOutput(ns('etikett_aldst'), inline = TRUE), 'box_aldst'),
              kpi('Andel kvinnor', 'box_kvinnor'),
              kpi('Andel utrikes födda', 'box_utrikes')),

          kort('Ålder per yrke',
               paste0('De 20 största yrkena i urvalet, sorterade efter andelen i den ',
                      'äldsta åldersgruppen – en indikation på kommande pensionsavgångar.'),
               'plot_alder'),
          kort('Kön per yrke',
               'De 20 största yrkena i urvalet, sorterade efter andelen kvinnor.',
               'plot_kon'),
          kort('Bakgrund per yrke',
               'De 20 största yrkena i urvalet, sorterade efter andelen utrikes födda.',
               'plot_bakgrund')
      )
  )
}

mod_demografi_server <- function(id, aktiv = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {

    urval <- mod_urval_yrke_server('urval', aktiv)

    profil <- shiny::reactive(
      hamta_yrke_profil(urval$ar(), urval$geografi(), urval$branschkoder(),
                        c('alder', 'kon', 'bakgrund'))
    ) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$branschkoder(), 'demografi')

    aldersgrupper <- shiny::reactive(sort(unique(profil()$alder)))
    aldst         <- shiny::reactive(utils::tail(aldersgrupper(), 1))
    utrikes       <- shiny::reactive(grep('^utrikes', unique(profil()$bakgrund),
                                          ignore.case = TRUE, value = TRUE))

    pct <- function(x) if (is.na(x)) '–' else scales::percent(x, accuracy = 0.1, decimal.mark = ',')

    output$etikett_aldst <- renderText(paste('Andel', aldst()))
    output$box_aldst     <- renderText(pct(andel_av_total(profil(), 'alder', aldst())))
    output$box_kvinnor   <- renderText(pct(andel_av_total(profil(), 'kon', 'Kvinna')))
    output$box_utrikes   <- renderText(pct(andel_av_total(profil(), 'bakgrund', utrikes())))

    fordelning <- function(kat_kol, kat, farger, sortera, sort_txt) {
      skapa_diagram_fordelning_per_yrke(
        fordelning_per_yrke(profil(), kat_kol), kat, farger,
        sortera_kategori = sortera,
        underrubrik      = paste0(urval$underrubrik(), ' · sorterat efter ', sort_txt),
        kalla            = KALLA_YRKE_UTB
      )
    }

    output$plot_alder <- renderGirafe({
      kat <- aldersgrupper()
      fordelning('alder', kat, stats::setNames(rd_sekventiell(length(kat)), kat),
                 aldst(), paste('andel', aldst()))
    })

    output$plot_kon <- renderGirafe({
      fordelning('kon', c('Kvinna', 'Man'),
                 c('Kvinna' = unname(KON_FARGER['Kvinnor']), 'Man' = unname(KON_FARGER['Män'])),
                 'Kvinna', 'andel kvinnor')
    })

    output$plot_bakgrund <- renderGirafe({
      # Utrikes födda först, så att sorteringen och färgen (blå) följs åt.
      kat <- c(utrikes(), setdiff(sort(unique(profil()$bakgrund)), utrikes()))
      fordelning('bakgrund', kat,
                 stats::setNames(c(RD_KATEGORISK_2, rep('grey70', 8))[seq_along(kat)], kat),
                 utrikes()[1], 'andel utrikes födda')
    })
  })
}
