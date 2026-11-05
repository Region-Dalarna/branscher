# =====================================================================
#  mod_utbildning_yrken.R – flik: Utbildning & yrken
#
#  Innehåll:
#   - Gemensamma val (mod_urval_yrke) + utbildningsindelning, kön och
#     ålder (Totalt eller -39 år)
#   - Nyckeltal: sysselsatta, antal yrken, antal utbildningsgrupper,
#     rekryteringsbredd (median antal utbildningsgrupper för 80 %)
#   - Mosaik: vilka utbildningar har de som arbetar inom valt yrke?
#   - Mosaik: inom vilka yrken arbetar de med vald utbildning?
#     Båda med tabell över de fem vanligaste (vald geografi + riket).
#     Klick på en ruta väljer den i den andra mosaiken.
#
#  Data hämtas aggregerad från databasen per urval (se
#  func_data_yrke_utb.R), och först när fliken öppnats.
# =====================================================================

UTB_YRKE_TOTALT  <- 'totalt'
UTB_YRKE_MAX_UNG <- 39  # åldersvalet "-39 år" = åldersgrupper med övre gräns <= 39

mod_utbildning_yrken_ui <- function(id) {
  ns <- NS(id)

  div(class = 'rd-app',

      mod_urval_yrke_ui(
        ns('urval'),
        div(class = 'rd-field',
            selectInput(ns('utb_indelning_val'), 'Utbildningsindelning',
                        choices = stats::setNames(UTB_INDELNINGAR$kod_kol,
                                                  UTB_INDELNINGAR$namn),
                        selected = UTB_INDELNINGAR$kod_kol[1])),
        div(class = 'rd-field',
            tags$label('Kön'),
            shinyWidgets::radioGroupButtons(
              ns('kon_val'), label = NULL, individual = TRUE, size = 'sm',
              choices = c('Totalt' = UTB_YRKE_TOTALT, 'Kvinnor' = 'Kvinna', 'Män' = 'Man'),
              selected = UTB_YRKE_TOTALT)),
        div(class = 'rd-field',
            tags$label('Ålder'),
            shinyWidgets::radioGroupButtons(
              ns('alder_val'), label = NULL, individual = TRUE, size = 'sm',
              choices = stats::setNames(c(UTB_YRKE_TOTALT, UTB_YRKE_MAX_UNG),
                                        c('Totalt', paste0('-', UTB_YRKE_MAX_UNG, ' år'))),
              selected = UTB_YRKE_TOTALT))
      ),

      div(class = 'rd-main',

          div(class = 'rd-kpi-row rd-kpi-row--4',
              rd_kpi('Sysselsatta', textOutput(ns('box_sysselsatta')),
                     paste('Antal sysselsatta med arbetsst\u00e4lle i vald geografi och bransch,',
                           'och i valt k\u00f6n och \u00e5lder.')),
              rd_kpi('Antal yrken', textOutput(ns('box_yrken')),
                     paste('Antal yrken (SSYK 2012, tresiffrig niv\u00e5) med minst en',
                           'sysselsatt i urvalet. Ok\u00e4nt yrke r\u00e4knas inte.')),
              rd_kpi('Antal utbildningsgrupper', textOutput(ns('box_utb')),
                     paste('Antal utbildningsgrupper, enligt vald utbildningsindelning,',
                           'med minst en sysselsatt i urvalet.')),
              rd_kpi('Rekryteringsbredd (median)', textOutput(ns('box_bredd')),
                     tagList(
                       'Hur m\u00e5nga utbildningsgrupper som beh\u00f6vs f\u00f6r att t\u00e4cka 80 % av ',
                       'de sysselsatta i ett yrke. V\u00e4rdet \u00e4r medianen \u00f6ver yrkena i urvalet ',
                       '(yrken med minst ', MIN_NAMNARE, ' sysselsatta).', tags$br(), tags$br(),
                       'Exempel: 3 betyder att 80 % av de sysselsatta i ett typiskt yrke har ',
                       'n\u00e5gon av bara tre utbildningar \u2013 yrkena rekryterar smalt. ',
                       'Ett h\u00f6gt v\u00e4rde betyder att yrkena rekryterar fr\u00e5n m\u00e5nga ',
                       'olika utbildningar.'))
          ),

          div(class = 'rd-card',
              h2('Vilka utbildningar har de som arbetar inom ett yrke?'),
              div(class = 'rd-subtitle',
                  'Rutornas yta visar hur stor andel av yrkets sysselsatta som har respektive ',
                  'utbildning. Klicka på en utbildning för att se dess yrken nedan.'),
              div(class = 'rd-field rd-field--kort',
                  selectizeInput(ns('yrke_val'), 'Yrke', choices = NULL, width = '100%',
                                 options = list(placeholder = 'Sök yrke…',
                                                sortField = '$order'))),
              girafeOutput(ns('plot_yrke_utb'), height = 'auto'),
              div(class = 'rd-topp',
                  h3('De fem vanligaste utbildningarna'),
                  uiOutput(ns('tabell_yrke_utb')))
          ),

          div(class = 'rd-card',
              h2('Inom vilka yrken arbetar de med en viss utbildning?'),
              div(class = 'rd-subtitle',
                  'Rutornas yta visar hur stor andel av dem med utbildningen som arbetar inom ',
                  'respektive yrke. Klicka på ett yrke för att se dess utbildningar ovan.'),
              div(class = 'rd-field rd-field--kort',
                  selectizeInput(ns('utb_val'), 'Utbildning', choices = NULL, width = '100%',
                                 options = list(placeholder = 'Sök utbildning…',
                                                sortField = '$order'))),
              girafeOutput(ns('plot_utb_yrke'), height = 'auto'),
              div(class = 'rd-topp',
                  h3('De fem vanligaste yrkena'),
                  uiOutput(ns('tabell_utb_yrke')))
          ),

      )
  )
}

# aktiv: reaktiv som är TRUE när fliken visas (se mod_urval_yrke.R).
# Returnerar list(yrke = reaktiv med valt yrke).
mod_utbildning_yrken_server <- function(id, aktiv = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {

    urval <- mod_urval_yrke_server('urval', aktiv)

    # ---- Data ------------------------------------------------------------

    # Kön/ålder som filter (villkor) på datafrågan; Totalt = inget filter.
    villkor <- shiny::reactive({
      shiny::req(input$kon_val, input$alder_val)
      v <- list()
      if (input$kon_val != UTB_YRKE_TOTALT) v$kon <- input$kon_val
      if (input$alder_val != UTB_YRKE_TOTALT) {
        v$alder <- aldersgrupper_upp_till(as.numeric(input$alder_val))
      }
      v
    })

    urvalstext <- shiny::reactive({
      delar <- c(
        if (input$kon_val == 'Kvinna') 'kvinnor' else if (input$kon_val == 'Man') 'män',
        if (input$alder_val != UTB_YRKE_TOTALT) paste0('-', input$alder_val, ' år')
      )
      if (length(delar) == 0) '' else paste0(' · ', paste(delar, collapse = ', '))
    })

    yrke_x_utb <- shiny::reactive({
      shiny::req(input$utb_indelning_val)
      hamta_yrke_x_utb(urval$ar(), urval$geografi(), urval$branschkoder(),
                       input$utb_indelning_val, villkor())
    }) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$branschkoder(),
                           input$utb_indelning_val, villkor())

    vald_x_utb <- shiny::reactive(
      dplyr::filter(yrke_x_utb(), kommun_kod == urval$geografi())
    )

    # ---- Val av yrke/utbildning --------------------------------------------
    # Listorna är sorterade efter antal sysselsatta (störst först). När
    # urvalet ändras ligger valt yrke/vald utbildning kvar om det har minst
    # MIN_NAMNARE sysselsatta i det nya urvalet; annars väljs det största.
    # (Byte av utbildningsindelning ger nya koder -> ny utbildning väljs.)
    # Undantag: med Alla branscher är grundskoleutbildning oftast störst
    # och säger lite -- då väljs den näst största utbildningen i stället.
    # Okänt finns kvar i listan men väljs aldrig som förval.
    # Listorna skickas till webbläsaren (server = FALSE) -- några hundra
    # alternativ -- så att ordningen behålls.

    shiny::observeEvent(vald_x_utb(), {
      yrken <- vald_x_utb() |>
        dplyr::filter(!yrke_kod %in% c('***', 'saknas')) |>
        dplyr::count(yrke_kod, yrke_namn, wt = antal, sort = TRUE)
      utb <- vald_x_utb() |>
        dplyr::count(utb_kod, utb_namn, wt = antal, sort = TRUE)

      # hoppa_over: funktion(namn) -> TRUE om den största kända ska
      # hoppas över till förmån för den näst största.
      forval <- function(kod, namn, hoppa_over = function(namn) FALSE) {
        kanda <- !.ar_okand(kod, namn)
        kand <- kod[kanda]
        if (length(kand) == 0) return(kod[1])
        if (length(kand) > 1 && hoppa_over(namn[kanda][1])) kand[2] else kand[1]
      }
      alla_branscher <- !nzchar(urval$bransch())
      grundskola <- function(namn) alla_branscher && grepl('grundskol', namn, ignore.case = TRUE)

      # Behåll nuvarande val om det har tillräckligt underlag i nya urvalet.
      behall <- function(val, tab, kod_kol, standard) {
        giltiga <- tab[[kod_kol]][tab$n >= MIN_NAMNARE]
        if (isTRUE(val %in% giltiga)) val else standard
      }

      updateSelectizeInput(session, 'yrke_val',
                           choices  = stats::setNames(yrken$yrke_kod, yrken$yrke_namn),
                           selected = behall(input$yrke_val, yrken, 'yrke_kod',
                                             forval(yrken$yrke_kod, yrken$yrke_namn)))
      updateSelectizeInput(session, 'utb_val',
                           choices  = stats::setNames(utb$utb_kod, utb$utb_namn),
                           selected = behall(input$utb_val, utb, 'utb_kod',
                                             forval(utb$utb_kod, utb$utb_namn, hoppa_over = grundskola)))
    })

    # Klick på en ruta väljer den i den andra mosaiken ("övr" = Övriga).
    shiny::observeEvent(input$plot_yrke_utb_selected, {
      val <- input$plot_yrke_utb_selected
      if (length(val) == 1 && !val %in% c('', 'övr')) {
        updateSelectizeInput(session, 'utb_val', selected = val)
      }
    })
    shiny::observeEvent(input$plot_utb_yrke_selected, {
      val <- input$plot_utb_yrke_selected
      if (length(val) == 1 && !val %in% c('', 'övr')) {
        updateSelectizeInput(session, 'yrke_val', selected = val)
      }
    })

    # ---- Nyckeltal -------------------------------------------------------

    output$box_sysselsatta <- renderText(
      formatera_nyckeltal(sum(vald_x_utb()$antal))
    )
    output$box_yrken <- renderText(
      dplyr::n_distinct(vald_x_utb()$yrke_kod[!vald_x_utb()$yrke_kod %in% c('***', 'saknas')])
    )
    output$box_utb <- renderText(
      dplyr::n_distinct(vald_x_utb()$utb_kod)
    )
    output$box_bredd <- renderText({
      b <- rekryteringsbredd_median(yrke_x_utb(), urval$geografi())
      if (is.na(b)) '–' else format(b, decimal.mark = ',')
    })

    # ---- Mosaiker + tabeller ---------------------------------------------

    andelar <- function(filter_kol, filter_varde, kat) {
      andel_inom(yrke_x_utb(), urval$geografi(), filter_kol, filter_varde, kat, topp_n = 30)
    }
    yrke_utb <- shiny::reactive({ shiny::req(input$yrke_val); andelar('yrke_kod', input$yrke_val, 'utb') })
    utb_yrke <- shiny::reactive({ shiny::req(input$utb_val);  andelar('utb_kod',  input$utb_val,  'yrke') })

    for_fa <- function(d) sum(d$antal[d$geo_niva == 'vald']) < MIN_NAMNARE
    tom_mosaik <- function() .girafe_std(
      .tom_plot(paste0('Färre än ', MIN_NAMNARE,
                       ' sysselsatta i urvalet – andelar visas inte')),
      height_svg = 1.5)

    mosaik <- function(d) {
      if (for_fa(d)) return(tom_mosaik())
      skapa_diagram_mosaik(
        d, geo_namn = urval$geo_namn(),
        underrubrik = paste0(urval$underrubrik(), urvalstext(), ' · ',
                             format(sum(d$antal[d$geo_niva == 'vald']), big.mark = ' '),
                             ' sysselsatta'),
        kalla = KALLA_YRKE_UTB)
    }
    tabell <- function(d, rubrik) {
      if (for_fa(d)) return(NULL)
      skapa_tabell_topp(d, rubrik, geo_namn = urval$geo_namn())
    }

    output$plot_yrke_utb   <- renderGirafe(mosaik(yrke_utb()))
    output$plot_utb_yrke   <- renderGirafe(mosaik(utb_yrke()))
    output$tabell_yrke_utb <- renderUI(tabell(yrke_utb(), 'Utbildning'))
    output$tabell_utb_yrke <- renderUI(tabell(utb_yrke(), 'Yrke'))

    # Valt yrke (SSYK 2012, tresiffrig) -- används av Behov & rekrytering.
    list(yrke = shiny::reactive(input$yrke_val))
  })
}
