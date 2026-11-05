# =====================================================================
#  mod_utbildning_yrken.R – flik: Utbildning & yrken
#
#  Innehåll:
#   - Gemensamma val (mod_urval_yrke) + utbildningsindelning, kön och
#     ålder (Totalt eller -39 år)
#   - Nyckeltal: sysselsatta, antal yrken, antal utbildningsgrupper,
#     rekryteringsbredd (median antal utbildningsgrupper för 80 %)
#   - Underflik Yrken och utbildningar:
#       - Mosaik: vilka utbildningar har de som arbetar inom valt yrke?
#       - Mosaik: inom vilka yrken arbetar de med vald utbildning?
#         Båda med tabell över de fem vanligaste (vald geografi + riket).
#         Klick på en ruta väljer den i den andra mosaiken.
#       - Staplar: inom vilka branscher arbetar de med valt yrke / vald
#         utbildning? Alla branscher, vald bransch framhävs.
#   - Underflik Per bransch: de största yrkena och utbildningarna i vald
#     bransch.
#   Alla stapeldiagram visar antal (de STAPEL_N största + egna tillägg),
#   totalt eller könsuppdelat.
#
#  Data hämtas aggregerad från databasen per urval (se
#  func_data_yrke_utb.R), och först när fliken öppnats.
# =====================================================================

UTB_YRKE_TOTALT  <- 'totalt'
UTB_YRKE_MAX_UNG <- 39  # åldersvalet "-39 år" = åldersgrupper med övre gräns <= 39
STAPEL_N         <- 15  # antal staplar (största) i stapeldiagrammen med antal

# Stapeldiagram med antal: Totalt/Könsuppdelat, sökruta för att lägga
# till egna (inputs <id>-yrken_val och <id>-bara_valda) och diagrammet.
stapel_ui <- function(ns, id, placeholder, bara_etikett) {
  tagList(
    div(class = 'rd-sortering',
        span(class = 'rd-sortering__label', 'Visa:'),
        shinyWidgets::radioGroupButtons(
          ns(paste0(id, '_kon')), label = NULL, individual = TRUE, size = 'sm',
          choices = c('Totalt' = 'totalt', 'Könsuppdelat' = 'kon'), selected = 'totalt'),
        conditionalPanel(
          sprintf("input.kon_val != '%s'", UTB_YRKE_TOTALT), ns = ns,
          span(class = 'rd-hint', 'Könsuppdelat går inte att välja när ett kön är valt i sidopanelen.'))),
    yrkesval_ui(NS(ns(id)), placeholder = placeholder, bara_etikett = bara_etikett),
    girafeOutput(ns(paste0(id, '_plot')), height = 'auto'))
}

STAPEL_ID <- c('stapel_bransch', 'stapel_br_yrke', 'stapel_br_utb')

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
                     paste('Antal sysselsatta med arbetsställe i vald geografi och bransch,',
                           'och i valt kön och ålder.')),
              rd_kpi('Antal yrken', textOutput(ns('box_yrken')),
                     paste('Antal yrken (SSYK 2012, tresiffrig nivå) med minst en',
                           'sysselsatt i urvalet. Okänt yrke räknas inte.')),
              rd_kpi('Antal utbildningsgrupper', textOutput(ns('box_utb')),
                     paste('Antal utbildningsgrupper, enligt vald utbildningsindelning,',
                           'med minst en sysselsatt i urvalet.')),
              rd_kpi('Rekryteringsbredd (median)', textOutput(ns('box_bredd')),
                     tagList(
                       'Hur många utbildningsgrupper som behövs för att täcka 80 % av ',
                       'de sysselsatta i ett yrke. Värdet är medianen över yrkena i urvalet ',
                       '(yrken med minst ', MIN_NAMNARE, ' sysselsatta).', tags$br(), tags$br(),
                       'Exempel: 3 betyder att 80 % av de sysselsatta i ett typiskt yrke har ',
                       'någon av bara tre utbildningar – yrkena rekryterar smalt. ',
                       'Ett högt värde betyder att yrkena rekryterar från många ',
                       'olika utbildningar.'))
          ),

          tabsetPanel(
            id = ns('underflik'),

            tabPanel('Yrken och utbildningar',
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

                     div(class = 'rd-card',
                         h2('Inom vilka branscher arbetar de?'),
                         div(class = 'rd-subtitle',
                             'Antal sysselsatta per bransch (vald branschindelning) för yrket eller ',
                             'utbildningen som är vald ovan. Alla branscher räknas, även om en bransch ',
                             'är vald i sidopanelen – den framhävs då. Branscher du lägger till ',
                             'framhävs och ersätter de minsta.'),
                         div(class = 'rd-sortering',
                             span(class = 'rd-sortering__label', 'Visa för:'),
                             shinyWidgets::radioGroupButtons(
                               ns('bransch_for'), label = NULL, individual = TRUE, size = 'sm',
                               choices = c('Valt yrke' = 'yrke', 'Vald utbildning' = 'utb'),
                               selected = 'yrke')),
                         stapel_ui(ns, 'stapel_bransch', 'Sök och lägg till branscher…',
                                   'Visa bara valda branscher'))
            ),

            tabPanel('Per bransch',
                     div(class = 'rd-card',
                         h2(textOutput(ns('rubrik_br_yrke'), inline = TRUE)),
                         div(class = 'rd-subtitle',
                             'Antal sysselsatta per yrke i den bransch som är vald i sidopanelen. ',
                             'Yrken du lägger till framhävs och ersätter de minsta.'),
                         stapel_ui(ns, 'stapel_br_yrke', 'Sök och lägg till yrken…',
                                   'Visa bara valda yrken')),
                     div(class = 'rd-card',
                         h2(textOutput(ns('rubrik_br_utb'), inline = TRUE)),
                         div(class = 'rd-subtitle',
                             'Antal sysselsatta per utbildning (vald utbildningsindelning) i den ',
                             'bransch som är vald i sidopanelen. Utbildningar du lägger till ',
                             'framhävs och ersätter de minsta.'),
                         stapel_ui(ns, 'stapel_br_utb', 'Sök och lägg till utbildningar…',
                                   'Visa bara valda utbildningar'))
            )
          )
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

    # En fråga med kön som extra dimension räcker för både mosaikerna
    # (kön summeras bort) och de könsuppdelade staplarna.
    yrke_x_utb_kon <- shiny::reactive({
      shiny::req(input$utb_indelning_val)
      hamta_yrke_x_utb(urval$ar(), urval$geografi(), urval$branschkoder(),
                       input$utb_indelning_val, villkor(), med_kon = TRUE)
    }) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$branschkoder(),
                           input$utb_indelning_val, villkor(), 'med_kon')

    yrke_x_utb <- shiny::reactive(utan_kon(yrke_x_utb_kon()))

    vald_x_utb <- shiny::reactive(
      dplyr::filter(yrke_x_utb(), kommun_kod == urval$geografi())
    )
    vald_x_utb_kon <- shiny::reactive(
      dplyr::filter(yrke_x_utb_kon(), kommun_kod == urval$geografi())
    )

    # Yrken/utbildningar i urvalet, störst först.
    yrken_lista <- shiny::reactive(
      vald_x_utb() |>
        dplyr::filter(!yrke_kod %in% c('***', 'saknas')) |>
        dplyr::count(yrke_kod, yrke_namn, wt = antal, sort = TRUE)
    )
    utb_lista <- shiny::reactive(
      vald_x_utb() |> dplyr::count(utb_kod, utb_namn, wt = antal, sort = TRUE)
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
      yrken <- yrken_lista()
      utb   <- utb_lista()

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

      # Sökrutorna i Per bransch: yrken resp. utbildningar att lägga till.
      updateSelectizeInput(session, 'stapel_br_yrke-yrken_val',
                           choices  = stats::setNames(yrken$yrke_kod, yrken$yrke_namn),
                           selected = intersect(input[['stapel_br_yrke-yrken_val']], yrken$yrke_kod))
      updateSelectizeInput(session, 'stapel_br_utb-yrken_val',
                           choices  = stats::setNames(utb$utb_kod, utb$utb_namn),
                           selected = intersect(input[['stapel_br_utb-yrken_val']], utb$utb_kod))
    })

    # Branscher att lägga till: alla grupper i vald indelning.
    branscher <- shiny::reactive(hamta_grupper_for_indelning(urval$indelning()))
    shiny::observeEvent(branscher(), {
      updateSelectizeInput(session, 'stapel_bransch-yrken_val',
                           choices  = branscher()$grupp_namn,
                           selected = intersect(input[['stapel_bransch-yrken_val']],
                                                branscher()$grupp_namn))
    })

    # Könsuppdelat går inte när ett kön är valt i sidopanelen.
    shiny::observeEvent(input$kon_val, ignoreInit = TRUE, {
      sparra <- input$kon_val != UTB_YRKE_TOTALT
      for (id in paste0(STAPEL_ID, '_kon')) {
        shinyWidgets::updateRadioGroupButtons(
          session, id,
          selected        = if (sparra) 'totalt' else input[[id]],
          disabledChoices = if (sparra) 'kon' else character(0))
      }
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

    # ---- Stapeldiagram med antal ------------------------------------------
    # d: enhet_kod, enhet_namn, kon, antal. alla: det som går att lägga till.
    stapel <- function(id, d, alla, vad, underrubrik, markerad = NULL) {
      valda <- input[[paste0(id, '-yrken_val')]] %||% character(0)
      bara  <- isTRUE(input[[paste0(id, '-bara_valda')]])
      if (bara && length(valda) == 0) {
        return(.girafe_std(.tom_plot(paste('Lägg till', vad, 'i sökrutan ovan')),
                           height_svg = 1.5))
      }
      t <- antal_topp(d, alla, n = STAPEL_N, valda = valda, bara_valda = bara,
                      kon_uppdelat = identical(input[[paste0(id, '_kon')]], 'kon'))
      skapa_diagram_antal_inom(
        t, markerad = c(if (!bara) valda, markerad),
        underrubrik = paste0(underrubrik, urvalstext(), ' · ',
                             if (bara) paste('valda', vad) else paste(STAPEL_N, 'största', vad)),
        kalla = KALLA_YRKE_UTB)
    }

    # Branscher för valt yrke / vald utbildning -- alla branscher, inte
    # filtrerat på vald bransch (egen fråga, liten: ett yrke/en utbildning).
    bransch_for <- shiny::reactive({
      shiny::req(input$bransch_for)
      if (input$bransch_for == 'yrke') {
        shiny::req(input$yrke_val)
        l <- yrken_lista()
        list(kol = 'ssyk3_2012', kod = input$yrke_val,
             namn = l$yrke_namn[match(input$yrke_val, l$yrke_kod)])
      } else {
        shiny::req(input$utb_val, input$utb_indelning_val)
        l <- utb_lista()
        list(kol = input$utb_indelning_val, kod = input$utb_val,
             namn = l$utb_namn[match(input$utb_val, l$utb_kod)])
      }
    })
    bransch_kon <- shiny::reactive({
      b <- bransch_for()
      if (b$kod == 'saknas') return(NULL)
      hamta_bransch_profil(urval$ar(), urval$geografi(), urval$indelning(), 'kon',
                           villkor = c(villkor(), stats::setNames(list(b$kod), b$kol)))
    }) |> shiny::bindCache(urval$ar(), urval$geografi(), urval$indelning(), villkor(),
                           bransch_for()$kol, bransch_for()$kod)

    output$stapel_bransch_plot <- renderGirafe({
      b <- bransch_for()
      d <- bransch_kon()
      if (is.null(d)) {
        return(.girafe_std(.tom_plot('Branscher kan inte visas för Uppgift saknas'),
                           height_svg = 1.5))
      }
      alla <- tibble::tibble(enhet_kod = branscher()$grupp_namn, enhet_namn = branscher()$grupp_namn)
      stapel('stapel_bransch', dplyr::select(d, enhet_kod, enhet_namn, kon, antal), alla,
             'branscher',
             paste0(b$namn, ' · ', urval$geo_namn(), ' · år ', urval$ar()),
             markerad = urval$bransch())
    })

    # Per bransch: största yrken / utbildningar i vald bransch.
    bransch_txt <- shiny::reactive(if (nzchar(urval$bransch())) urval$bransch() else 'alla branscher')
    output$rubrik_br_yrke <- renderText(paste('De största yrkena i', bransch_txt()))
    output$rubrik_br_utb  <- renderText(paste('De största utbildningarna i', bransch_txt()))

    output$stapel_br_yrke_plot <- renderGirafe({
      d <- vald_x_utb_kon() |>
        dplyr::transmute(enhet_kod = yrke_kod, enhet_namn = yrke_namn, kon, antal)
      alla <- dplyr::transmute(yrken_lista(), enhet_kod = yrke_kod, enhet_namn = yrke_namn)
      stapel('stapel_br_yrke', d, alla, 'yrken', urval$underrubrik())
    })
    output$stapel_br_utb_plot <- renderGirafe({
      d <- vald_x_utb_kon() |>
        dplyr::transmute(enhet_kod = utb_kod, enhet_namn = utb_namn, kon, antal)
      alla <- dplyr::transmute(utb_lista(), enhet_kod = utb_kod, enhet_namn = utb_namn)
      stapel('stapel_br_utb', d, alla, 'utbildningar', urval$underrubrik())
    })

    # Valt yrke (SSYK 2012, tresiffrig) -- används av Behov & rekrytering.
    list(yrke = shiny::reactive(input$yrke_val))
  })
}
