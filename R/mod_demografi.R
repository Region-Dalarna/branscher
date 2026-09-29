# =====================================================================
#  mod_demografi.R – flik: Demografi
#
#  Ålder, kön och bakgrund bland de sysselsatta
#  (mikro_db.utb_yrken_branscher).
#   - Nyckeltal för urvalet: andel i valda åldersgrupper, andel av valt
#     kön och andel med vald bakgrund (följer sorteringsvalen nedan)
#   - Underflik Branscher (förvald): ålder/kön/bakgrund per bransch i
#     vald indelning. Alla branscher visas; vald bransch framhävs.
#   - Underflik Yrken: samma diagram för de 20 största yrkena i urvalet
#     (filtrerat på vald bransch).
#
#  Sortering: varje diagram har en rad med knappar ("Sortera efter
#  andel"). Ålder: en eller flera åldersgrupper (förval 68+). Kön och
#  bakgrund: ett av två (förval kvinnor resp. utrikes födda). Valen är
#  gemensamma för underflikarna -- knapparna i Branscher och Yrken hålls
#  i synk.
# =====================================================================

KON_VAL <- c('Kvinnor' = 'Kvinna', 'Män' = 'Man')
# Startvärden innan datat lästs (radioGroupButtons kräver minst ett val);
# ersätts av värdena i tabellen när fliken öppnas.
BAKGRUND_VAL <- c('Utrikes födda' = 'Utrikes född', 'Inrikes födda' = 'Inrikes född')

mod_demografi_ui <- function(id) {
  ns <- NS(id)

  sortering <- function(kontroll) {
    div(class = 'rd-sortering',
        span(class = 'rd-sortering__label', 'Sortera efter andel:'),
        kontroll)
  }
  knappar_flera <- function(input_id) {
    shinyWidgets::checkboxGroupButtons(ns(input_id), label = NULL, choices = character(0),
                                       individual = TRUE, size = 'sm')
  }
  knappar_en <- function(input_id, choices, selected) {
    shinyWidgets::radioGroupButtons(ns(input_id), label = NULL, choices = choices,
                                    selected = selected, individual = TRUE, size = 'sm')
  }
  kort <- function(rubrik, underrubrik, kontroll, output_id) {
    div(class = 'rd-card',
        h2(rubrik),
        div(class = 'rd-subtitle', underrubrik),
        sortering(kontroll),
        girafeOutput(ns(output_id), height = 'auto'))
  }
  diagram <- function(enhet, enheter_txt, suffix) {
    tagList(
      kort(paste('Ålder per', enhet), enheter_txt,
           knappar_flera(paste0('sort_alder_', suffix)),
           paste0('plot_alder_', suffix)),
      kort(paste('Kön per', enhet), enheter_txt,
           knappar_en(paste0('sort_kon_', suffix), KON_VAL, 'Kvinna'),
           paste0('plot_kon_', suffix)),
      kort(paste('Bakgrund per', enhet), enheter_txt,
           knappar_en(paste0('sort_bakgrund_', suffix), BAKGRUND_VAL, BAKGRUND_VAL[[1]]),
           paste0('plot_bakgrund_', suffix))
    )
  }

  div(class = 'rd-app',
      mod_urval_yrke_ui(ns('urval')),

      div(class = 'rd-main',
          div(class = 'rd-kpi-row',
              rd_kpi(textOutput(ns('etikett_alder'), inline = TRUE), textOutput(ns('box_alder')),
                     textOutput(ns('forklaring_alder'), inline = TRUE)),
              rd_kpi(textOutput(ns('etikett_kon'), inline = TRUE), textOutput(ns('box_kon')),
                     textOutput(ns('forklaring_kon'), inline = TRUE)),
              rd_kpi(textOutput(ns('etikett_bakgrund'), inline = TRUE), textOutput(ns('box_bakgrund')),
                     textOutput(ns('forklaring_bakgrund'), inline = TRUE))),

          tabsetPanel(
            id = ns('underflik'),
            tabPanel('Branscher',
                     diagram('bransch',
                             'Alla branscher i vald branschindelning (vald bransch framhävs).',
                             'bransch')),
            tabPanel('Yrken',
                     yrkesval_ui(ns, 'S\u00f6k och l\u00e4gg till yrken i diagrammen\u2026',
                                 class = 'rd-yrkesval--underflik'),
                     diagram('yrke',
                             paste('De 20 största yrkena i urvalet. Yrken du lägger till ovan',
                                   'framhävs och ersätter de minsta \u2013 eller visas ensamma med',
                                   '\u201cVisa bara valda yrken\u201d.'),
                             'yrke'))
          )
      )
  )
}

mod_demografi_server <- function(id, aktiv = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {

    urval <- mod_urval_yrke_server('urval', aktiv)
    kat_kolumner <- c('alder', 'kon', 'bakgrund')
    suffix <- c('bransch', 'yrke')

    # Yrken i urvalet (filtrerat på vald bransch) -- används även för
    # nyckeltalen, eftersom summan över yrken = hela urvalet.
    profil_yrke <- shiny::reactive(
      hamta_yrke_profil(urval$ar(), urval$geografi(), urval$branschkoder(), kat_kolumner)
    ) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$branschkoder(), 'demografi')

    # Alla branscher i vald indelning (inte filtrerat på vald bransch).
    profil_bransch <- shiny::reactive(
      hamta_bransch_profil(urval$ar(), urval$geografi(), urval$indelning(), kat_kolumner)
    ) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$indelning(), 'demografi_bransch')

    aldersgrupper <- shiny::reactive(sortera_aldersgrupper(profil_yrke()$alder))
    # Bakgrunder med utrikes född först (förval och blå färg).
    bakgrunder <- shiny::reactive({
      b <- sort(unique(profil_yrke()$bakgrund))
      utr <- grepl('^utrikes', b, ignore.case = TRUE)
      c(b[utr], b[!utr])
    })

    # ---- Sorteringsval (gemensamma för underflikarna) --------------------

    sort_alder    <- shiny::reactiveVal(character(0))
    sort_kon      <- shiny::reactiveVal('Kvinna')
    sort_bakgrund <- shiny::reactiveVal(NULL)

    # Knapparnas etiketter: "Utrikes född" -> "Utrikes födda".
    bakgrund_val <- shiny::reactive(stats::setNames(bakgrunder(), sub('född$', 'födda', bakgrunder())))

    shiny::observeEvent(aldersgrupper(), {
      val <- intersect(sort_alder(), aldersgrupper())
      if (length(val) == 0) val <- alder_forval(aldersgrupper())
      sort_alder(val)
      for (s in suffix) {
        shinyWidgets::updateCheckboxGroupButtons(session, paste0('sort_alder_', s),
                                                 choices = aldersgrupper(), selected = val,
                                                 size = 'sm')
      }
    })

    shiny::observeEvent(bakgrunder(), {
      val <- if (isTRUE(sort_bakgrund() %in% bakgrunder())) sort_bakgrund() else bakgrunder()[1]
      sort_bakgrund(val)
      for (s in suffix) {
        shinyWidgets::updateRadioGroupButtons(session, paste0('sort_bakgrund_', s),
                                              choices = bakgrund_val(), selected = val,
                                              size = 'sm')
      }
    })

    # Ett val i en underflik sparas och speglas till den andra. Ålder får
    # vara tom (ignoreNULL = FALSE) -- då sorteras efter storlek.
    synka <- function(namn, rv, uppdatera, tom_tillaten = FALSE) {
      for (s in suffix) local({
        egen <- paste0(namn, s)
        andra <- paste0(namn, setdiff(suffix, s))
        shiny::observeEvent(input[[egen]], ignoreInit = TRUE, ignoreNULL = !tom_tillaten, {
          val <- input[[egen]] %||% character(0)
          if (identical(sort(val), sort(rv()))) return()
          rv(val)
          uppdatera(session, andra, selected = val)
        })
      })
    }
    synka('sort_alder_', sort_alder, shinyWidgets::updateCheckboxGroupButtons, tom_tillaten = TRUE)
    synka('sort_kon_', sort_kon, shinyWidgets::updateRadioGroupButtons)
    synka('sort_bakgrund_', sort_bakgrund, shinyWidgets::updateRadioGroupButtons)

    # ---- Texter som följer valen ----------------------------------------

    alder_txt <- shiny::reactive(alder_etikett(sort_alder(), aldersgrupper()))
    kon_txt   <- shiny::reactive(c(Kvinna = 'kvinnor', Man = 'män')[[sort_kon()]])
    bakgrund_txt <- shiny::reactive({
      shiny::req(sort_bakgrund())
      tolower(sub('född$', 'födda', sort_bakgrund()))
    })

    # ---- Nyckeltal -------------------------------------------------------

    pct <- function(x) if (is.na(x)) '–' else scales::percent(x, accuracy = 0.1, decimal.mark = ',')

    output$etikett_alder <- renderText(
      if (length(sort_alder()) == 0) 'Andel i vald ålder' else paste('Andel', alder_txt())
    )
    output$box_alder <- renderText(
      if (length(sort_alder()) == 0) '–'
      else pct(andel_av_total(profil_yrke(), 'alder', sort_alder()))
    )
    output$forklaring_alder <- renderText(paste(
      'Andel av de sysselsatta i urvalet (vald geografi och bransch) i de åldersgrupper',
      'som är valda för sorteringen av åldersdiagrammet. De äldsta grupperna ger en',
      'indikation på kommande pensionsavgångar.'))

    output$etikett_kon <- renderText(paste('Andel', kon_txt()))
    output$box_kon     <- renderText(pct(andel_av_total(profil_yrke(), 'kon', sort_kon())))
    output$forklaring_kon <- renderText(paste0(
      'Andel ', kon_txt(), ' bland de sysselsatta i urvalet (vald geografi och bransch). ',
      'Följer valet i könsdiagrammets sortering.'))

    output$etikett_bakgrund <- renderText(paste('Andel', bakgrund_txt()))
    output$box_bakgrund     <- renderText(pct(andel_av_total(profil_yrke(), 'bakgrund', sort_bakgrund())))
    output$forklaring_bakgrund <- renderText(paste0(
      'Andel ', bakgrund_txt(), ' bland de sysselsatta i urvalet (vald geografi och bransch). ',
      'Följer valet i bakgrundsdiagrammets sortering.'))

    # ---- Diagram (samma tre för branscher och yrken) --------------------

    fordelning <- function(profil, n, markerad, underrubrik, kat_kol, kat, farger, sortera, sort_txt,
                           enheter = NULL) {
      if (n == 0 && length(enheter) == 0) {
        return(.girafe_std(.tom_plot('L\u00e4gg till yrken i s\u00f6krutan ovan'), height_svg = 1.5))
      }
      skapa_diagram_fordelning(
        fordelning_per_enhet(profil, kat_kol, n = n, enheter = enheter), kat, farger,
        markerad         = markerad,
        sortera_kategori = sortera,
        underrubrik      = paste0(underrubrik, ' · sorterat efter ', sort_txt),
        kalla            = KALLA_YRKE_UTB
      )
    }

    # n, markerad, underrubrik och enheter är funktioner (reaktiva).
    # enheter: enheter som läggs till bland de n största (n = 0: bara de).
    rita <- function(profil, n, markerad, underrubrik, enheter = function() NULL) {
      list(
        alder = function() {
          kat <- aldersgrupper()
          fordelning(profil(), n(), markerad(), underrubrik(), 'alder', kat,
                     stats::setNames(rd_sekventiell(length(kat)), kat),
                     sort_alder(),
                     if (length(sort_alder()) == 0) 'storlek' else paste('andel', alder_txt()),
                     enheter())
        },
        kon = function() {
          fordelning(profil(), n(), markerad(), underrubrik(), 'kon', unname(KON_VAL),
                     c('Kvinna' = unname(KON_FARGER['Kvinnor']), 'Man' = unname(KON_FARGER['Män'])),
                     sort_kon(), paste('andel', kon_txt()), enheter())
        },
        bakgrund = function() {
          kat <- bakgrunder()
          fordelning(profil(), n(), markerad(), underrubrik(), 'bakgrund', kat,
                     stats::setNames(c(RD_KATEGORISK_2, rep('grey70', 8))[seq_along(kat)], kat),
                     sort_bakgrund(), paste('andel', bakgrund_txt()), enheter())
        }
      )
    }

    # Branschdiagrammen visar alla branscher -- underrubriken nämner
    # därför inte vald bransch.
    bransch <- rita(profil_bransch, n = function() Inf, markerad = urval$bransch,
                    underrubrik = function() paste0(urval$geo_namn(), ' · år ', urval$ar()))
    # Yrken: valda yrken läggs till bland de 20 största och framhävs --
    # eller visas ensamma ("Visa bara valda yrken").
    valda_yrken <- shiny::reactive(input$yrken_val %||% character(0))
    bara_valda  <- shiny::reactive(isTRUE(input$bara_valda))
    yrke    <- rita(profil_yrke,
                    n        = function() if (bara_valda()) 0 else 20,
                    markerad = function() if (bara_valda()) NULL else valda_yrken(),
                    enheter  = valda_yrken,
                    underrubrik = function() paste0(
                      urval$underrubrik(), notis_for_sma(antal_for_sma(profil_yrke(), valda_yrken()))))

    shiny::observeEvent(profil_yrke(), {
      yrken <- profil_yrke() |>
        dplyr::filter(!enhet_kod %in% c('***', 'saknas')) |>
        dplyr::count(enhet_kod, enhet_namn, wt = antal, sort = TRUE)
      updateSelectizeInput(session, 'yrken_val',
                           choices  = stats::setNames(yrken$enhet_kod, yrken$enhet_namn),
                           selected = intersect(input$yrken_val, yrken$enhet_kod))
    })

    output$plot_alder_bransch    <- renderGirafe(bransch$alder())
    output$plot_kon_bransch      <- renderGirafe(bransch$kon())
    output$plot_bakgrund_bransch <- renderGirafe(bransch$bakgrund())
    output$plot_alder_yrke       <- renderGirafe(yrke$alder())
    output$plot_kon_yrke         <- renderGirafe(yrke$kon())
    output$plot_bakgrund_yrke    <- renderGirafe(yrke$bakgrund())
  })
}
