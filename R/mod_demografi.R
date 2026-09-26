# =====================================================================
#  mod_demografi.R – flik: Demografi
#
#  Ålder, kön och bakgrund bland de sysselsatta
#  (mikro_db.utb_yrken_branscher).
#   - Nyckeltal för urvalet: andel i äldsta åldersgruppen, andel
#     kvinnor, andel utrikes födda
#   - Underflik Branscher (förvald): ålder/kön/bakgrund per bransch i
#     vald indelning. Alla branscher visas; vald bransch framhävs.
#   - Underflik Yrken: samma diagram för de 20 största yrkena i urvalet
#     (filtrerat på vald bransch).
#  Diagrammen i en dold underflik ritas inte förrän den visas.
# =====================================================================

mod_demografi_ui <- function(id) {
  ns <- NS(id)

  kort <- function(rubrik, underrubrik, output_id) {
    div(class = 'rd-card',
        h2(rubrik),
        div(class = 'rd-subtitle', underrubrik),
        girafeOutput(ns(output_id), height = 'auto'))
  }
  diagram <- function(enhet, enheter_txt, suffix) {
    tagList(
      kort(paste('Ålder per', enhet),
           paste0(enheter_txt, ', sorterade efter andelen i den äldsta åldersgruppen ',
                  '– en indikation på kommande pensionsavgångar.'),
           paste0('plot_alder_', suffix)),
      kort(paste('Kön per', enhet),
           paste0(enheter_txt, ', sorterade efter andelen kvinnor.'),
           paste0('plot_kon_', suffix)),
      kort(paste('Bakgrund per', enhet),
           paste0(enheter_txt, ', sorterade efter andelen utrikes födda.'),
           paste0('plot_bakgrund_', suffix))
    )
  }

  div(class = 'rd-app',
      mod_urval_yrke_ui(ns('urval')),

      div(class = 'rd-main',
          div(class = 'rd-kpi-row',
              rd_kpi(textOutput(ns('etikett_aldst'), inline = TRUE), textOutput(ns('box_aldst')),
                     textOutput(ns('forklaring_aldst'), inline = TRUE)),
              rd_kpi('Andel kvinnor', textOutput(ns('box_kvinnor')),
                     'Andel kvinnor bland de sysselsatta i urvalet (vald geografi och bransch).'),
              rd_kpi('Andel utrikes födda', textOutput(ns('box_utrikes')),
                     'Andel utrikes födda bland de sysselsatta i urvalet (vald geografi och bransch).')),

          tabsetPanel(
            id = ns('underflik'),
            tabPanel('Branscher',
                     diagram('bransch',
                             'Alla branscher i vald branschindelning (vald bransch framhävs)',
                             'bransch')),
            tabPanel('Yrken',
                     diagram('yrke', 'De 20 största yrkena i urvalet', 'yrke'))
          )
      )
  )
}

mod_demografi_server <- function(id, aktiv = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {

    urval <- mod_urval_yrke_server('urval', aktiv)
    kat_kolumner <- c('alder', 'kon', 'bakgrund')

    # Yrken i urvalet (filtrerat på vald bransch) -- används även för
    # nyckeltalen, eftersom summan över yrken = hela urvalet.
    profil_yrke <- shiny::reactive(
      hamta_yrke_profil(urval$ar(), urval$geografi(), urval$branschkoder(), kat_kolumner)
    ) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$branschkoder(), 'demografi')

    # Alla branscher i vald indelning (inte filtrerat på vald bransch).
    profil_bransch <- shiny::reactive(
      hamta_bransch_profil(urval$ar(), urval$geografi(), urval$indelning(), kat_kolumner)
    ) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$indelning(), 'demografi_bransch')

    aldersgrupper <- shiny::reactive(sort(unique(profil_yrke()$alder)))
    aldst         <- shiny::reactive(utils::tail(aldersgrupper(), 1))
    utrikes       <- shiny::reactive(grep('^utrikes', unique(profil_yrke()$bakgrund),
                                          ignore.case = TRUE, value = TRUE))

    # ---- Nyckeltal -------------------------------------------------------

    pct <- function(x) if (is.na(x)) '–' else scales::percent(x, accuracy = 0.1, decimal.mark = ',')

    output$etikett_aldst    <- renderText(paste('Andel', aldst()))
    output$forklaring_aldst <- renderText(paste0(
      'Andel av de sysselsatta i urvalet som är ', aldst(),
      ' – en indikation på hur stor del som går i pension de närmaste åren.'))
    output$box_aldst   <- renderText(pct(andel_av_total(profil_yrke(), 'alder', aldst())))
    output$box_kvinnor <- renderText(pct(andel_av_total(profil_yrke(), 'kon', 'Kvinna')))
    output$box_utrikes <- renderText(pct(andel_av_total(profil_yrke(), 'bakgrund', utrikes())))

    # ---- Diagram (samma tre för branscher och yrken) --------------------

    fordelning <- function(profil, n, markerad, underrubrik, kat_kol, kat, farger, sortera, sort_txt) {
      skapa_diagram_fordelning(
        fordelning_per_enhet(profil, kat_kol, n = n), kat, farger,
        markerad         = markerad,
        sortera_kategori = sortera,
        underrubrik      = paste0(underrubrik, ' · sorterat efter ', sort_txt),
        kalla            = KALLA_YRKE_UTB
      )
    }

    # underrubrik: funktion som ger diagrammens underrubrik.
    rita <- function(profil, n, markerad, underrubrik) {
      list(
        alder = function() {
          kat <- aldersgrupper()
          fordelning(profil(), n, markerad(), underrubrik(), 'alder', kat,
                     stats::setNames(rd_sekventiell(length(kat)), kat),
                     aldst(), paste('andel', aldst()))
        },
        kon = function() {
          fordelning(profil(), n, markerad(), underrubrik(), 'kon', c('Kvinna', 'Man'),
                     c('Kvinna' = unname(KON_FARGER['Kvinnor']), 'Man' = unname(KON_FARGER['Män'])),
                     'Kvinna', 'andel kvinnor')
        },
        bakgrund = function() {
          # Utrikes födda först, så att sorteringen och färgen (blå) följs åt.
          kat <- c(utrikes(), setdiff(sort(unique(profil()$bakgrund)), utrikes()))
          fordelning(profil(), n, markerad(), underrubrik(), 'bakgrund', kat,
                     stats::setNames(c(RD_KATEGORISK_2, rep('grey70', 8))[seq_along(kat)], kat),
                     utrikes()[1], 'andel utrikes födda')
        }
      )
    }

    # Branschdiagrammen visar alla branscher -- underrubriken nämner
    # därför inte vald bransch.
    bransch <- rita(profil_bransch, n = Inf, markerad = urval$bransch,
                    underrubrik = function() paste0(urval$geo_namn(), ' · år ', urval$ar()))
    yrke    <- rita(profil_yrke, n = 20, markerad = function() NULL,
                    underrubrik = urval$underrubrik)

    output$plot_alder_bransch    <- renderGirafe(bransch$alder())
    output$plot_kon_bransch      <- renderGirafe(bransch$kon())
    output$plot_bakgrund_bransch <- renderGirafe(bransch$bakgrund())
    output$plot_alder_yrke       <- renderGirafe(yrke$alder())
    output$plot_kon_yrke         <- renderGirafe(yrke$kon())
    output$plot_bakgrund_yrke    <- renderGirafe(yrke$bakgrund())
  })
}
