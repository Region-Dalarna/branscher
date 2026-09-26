# =====================================================================
#  mod_utbildning_yrken.R – flik: Utbildning & yrken
#
#  Innehåll:
#   - Val av år, geografi, branschindelning + bransch, utbildnings-
#     indelning, samt yrke och utbildning för detaljdiagrammen
#   - Nyckeltal: sysselsatta, antal yrken, antal utbildningsgrupper,
#     rekryteringsbredd (median antal utbildningsgrupper för 80 %)
#   - Utbildningsnivå per yrke (klick på ett yrke väljer det nedan)
#   - Yrke -> utbildningar och utbildning -> yrken, med riket som jämförelse
#   - Matchning, ålder och kön per yrke (inre flikar)
#
#  Data hämtas aggregerad från databasen per urval (se
#  func_data_yrke_utb.R) -- tabellen läses inte in i sin helhet.
# =====================================================================

KALLA_YRKE_UTB <- 'SCB (Yrkesregistret, Utbildningsregistret), bearbetat av Region Dalarna'

mod_utbildning_yrken_ui <- function(id) {
  ns <- NS(id)

  div(class = 'rd-app',

      div(class = 'rd-sidebar',
          h3('Val'),

          div(class = 'rd-field',
              selectInput(ns('ar_val'), 'År', choices = NULL)),
          div(class = 'rd-field',
              selectInput(ns('geografi_val'), 'Geografi', choices = NULL)),
          div(class = 'rd-field',
              selectInput(ns('indelning_val'), 'Branschindelning', choices = NULL)),
          div(class = 'rd-field',
              selectInput(ns('bransch_val'), 'Bransch',
                          choices = c('Alla branscher' = ''))),
          div(class = 'rd-field',
              selectInput(ns('utb_indelning_val'), 'Utbildningsindelning',
                          choices = stats::setNames(UTB_INDELNINGAR$kod_kol,
                                                    UTB_INDELNINGAR$namn))),
          div(class = 'rd-field',
              selectizeInput(ns('yrke_val'), 'Yrke', choices = NULL,
                             options = list(placeholder = 'Sök yrke…'))),
          div(class = 'rd-field',
              selectizeInput(ns('utb_val'), 'Utbildning', choices = NULL,
                             options = list(placeholder = 'Sök utbildning…'))),

          div(class = 'rd-info',
              tags$strong('OBS: '),
              'Antal under 4 visas som “färre än 4”. Andelar visas bara ',
              'när underlaget är minst ', MIN_NAMNARE, ' sysselsätta.')
      ),

      div(class = 'rd-main',

          div(class = 'rd-kpi-row',
              div(class = 'rd-kpi',
                  div(class = 'rd-kpi__label', 'Sysselsätta'),
                  div(class = 'rd-kpi__value', textOutput(ns('box_sysselsatta')))),
              div(class = 'rd-kpi',
                  div(class = 'rd-kpi__label', 'Antal yrken'),
                  div(class = 'rd-kpi__value', textOutput(ns('box_yrken')))),
              div(class = 'rd-kpi',
                  div(class = 'rd-kpi__label', 'Antal utbildningsgrupper'),
                  div(class = 'rd-kpi__value', textOutput(ns('box_utb')))),
              div(class = 'rd-kpi',
                  div(class = 'rd-kpi__label', 'Rekryteringsbredd (median)'),
                  div(class = 'rd-kpi__value', textOutput(ns('box_bredd'))))
          ),

          div(class = 'rd-card',
              h2('Utbildningsnivå per yrke'),
              div(class = 'rd-subtitle',
                  'De 20 största yrkena i urvalet. Klicka på ett yrke för att se dess utbildningar nedan.'),
              girafeOutput(ns('plot_niva'), height = 'auto')
          ),

          div(class = 'rd-card',
              h2(textOutput(ns('rubrik_yrke'), inline = TRUE)),
              div(class = 'rd-subtitle',
                  'Andel av yrkets sysselsätta per utbildning. Stapel = vald geografi, grå punkt = riket.'),
              girafeOutput(ns('plot_yrke_utb'), height = 'auto')
          ),

          div(class = 'rd-card',
              h2(textOutput(ns('rubrik_utb'), inline = TRUE)),
              div(class = 'rd-subtitle',
                  'Andel av utbildningens sysselsätta per yrke. Stapel = vald geografi, grå punkt = riket.'),
              girafeOutput(ns('plot_utb_yrke'), height = 'auto')
          ),

          div(class = 'rd-card',
              h2('Yrkenas sammansättning'),
              div(class = 'rd-subtitle', 'De 20 största yrkena i urvalet.'),
              tabsetPanel(
                id = ns('profil_flik'),
                tabPanel('Matchning',
                         girafeOutput(ns('plot_matchning'), height = 'auto'),
                         h3('Anställda utan tillräckliga uppgifter per bransch'),
                         div(class = 'rd-subtitle',
                             'Andel av de anställda där yrkes- eller utbildningsuppgift saknas, ',
                             'så att matchning inte kan bedömas. Stapel = vald geografi, ',
                             'grå punkt = riket.'),
                         girafeOutput(ns('plot_utan_uppgifter'), height = 'auto')),
                tabPanel('Ålder',    girafeOutput(ns('plot_alder'),     height = 'auto')),
                tabPanel('Kön',      girafeOutput(ns('plot_kon'),       height = 'auto'))
              )
          ),

          div(class = 'rd-info',
              tags$strong('Rekryteringsbredd: '),
              'antal utbildningsgrupper som tillsammans täcker 80 % av ett yrkes ',
              'sysselsätta, median över yrkena i urvalet. Lågt värde = yrkena ',
              'rekryterar från få utbildningar; högt = bred rekryteringsbas.')
      )
  )
}

# aktiv: reaktiv som är TRUE när fliken visas. Fliken hämtar ingen data
# förrän den öppnats första gången -- alla datafrågor väntar (req) på
# valen som fylls i nedan, så appen startar utan att fråga databasen här.
mod_utbildning_yrken_server <- function(id, aktiv = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    indelningar <- hamta_indelningar()
    kommuner    <- hamta_kommuner()

    forsta_oppning <- shiny::reactive(if (isTRUE(aktiv())) TRUE else NULL)

    shiny::observeEvent(forsta_oppning(), once = TRUE, {
      ar_lista <- hamta_ar_lista_yrke_utb()
      updateSelectInput(session, 'ar_val', choices = ar_lista, selected = ar_lista[1])
      updateSelectInput(session, 'geografi_val',
                        choices = c('Hela Dalarna (länet)' = '20',
                                    'Riket' = '00',
                                    stats::setNames(kommuner$kommun_kod, kommuner$kommun_namn)))
      updateSelectInput(session, 'indelning_val',
                        choices  = stats::setNames(indelningar$kolumn, indelningar$namn),
                        selected = 'grupp_benamning')
    })

    shiny::observeEvent(input$indelning_val, {
      shiny::req(input$indelning_val)  # "" vid start, innan valen fyllts
      grp <- hamta_grupper_for_indelning(input$indelning_val)
      updateSelectInput(session, 'bransch_val',
                        choices = c('Alla branscher' = '',
                                    stats::setNames(grp$grupp_namn, grp$grupp_namn)))
    })

    branschkoder <- shiny::reactive({
      shiny::req(input$indelning_val)
      hamta_branschkoder(input$indelning_val, input$bransch_val)
    })

    bransch_txt <- shiny::reactive({
      if (nzchar(input$bransch_val %||% '')) input$bransch_val else 'Alla branscher'
    })

    geo_namn <- shiny::reactive({
      alla <- c('20' = 'Dalarna', '00' = 'Riket',
                stats::setNames(kommuner$kommun_namn, kommuner$kommun_kod))
      unname(alla[input$geografi_val])
    })

    underrubrik <- shiny::reactive(
      paste0(geo_namn(), ' · ', bransch_txt(), ' · år ', input$ar_val)
    )

    # ---- Data (två databasanrop per urval) -------------------------------

    yrke_x_utb <- shiny::reactive({
      shiny::req(input$ar_val, input$geografi_val, input$utb_indelning_val)
      hamta_yrke_x_utb(input$ar_val, input$geografi_val, branschkoder(),
                       input$utb_indelning_val)
    }) |> shiny::bindCache(input$ar_val, input$geografi_val, branschkoder(),
                           input$utb_indelning_val)

    profil <- shiny::reactive({
      shiny::req(input$ar_val, input$geografi_val)
      hamta_yrke_profil(input$ar_val, input$geografi_val, branschkoder())
    }) |> shiny::bindCache(input$ar_val, input$geografi_val, branschkoder())

    vald_x_utb <- shiny::reactive(
      dplyr::filter(yrke_x_utb(), kommun_kod == input$geografi_val)
    )

    # ---- Val av yrke/utbildning (sorterade efter storlek) ----------------

    shiny::observeEvent(vald_x_utb(), {
      yrken <- vald_x_utb() |>
        dplyr::filter(yrke_kod != '***') |>
        dplyr::count(yrke_kod, yrke_namn, wt = antal, sort = TRUE)
      utb <- vald_x_utb() |>
        dplyr::count(utb_kod, utb_namn, wt = antal, sort = TRUE)

      behall <- function(val, koder) if (isTRUE(val %in% koder)) val else koder[1]

      updateSelectizeInput(session, 'yrke_val', server = TRUE,
                           choices  = stats::setNames(yrken$yrke_kod, yrken$yrke_namn),
                           selected = behall(input$yrke_val, yrken$yrke_kod))
      updateSelectizeInput(session, 'utb_val', server = TRUE,
                           choices  = stats::setNames(utb$utb_kod, utb$utb_namn),
                           selected = behall(input$utb_val, utb$utb_kod))
    })

    # Klick på ett yrke i nivådiagrammet väljer det i yrkesväljaren.
    shiny::observeEvent(input$plot_niva_selected, {
      val <- input$plot_niva_selected
      if (length(val) == 1 && nzchar(val)) {
        updateSelectizeInput(session, 'yrke_val', selected = val)
      }
    })

    # ---- Nyckeltal -------------------------------------------------------

    output$box_sysselsatta <- renderText(
      formatera_nyckeltal(sum(vald_x_utb()$antal))
    )
    output$box_yrken <- renderText(
      dplyr::n_distinct(vald_x_utb()$yrke_kod[vald_x_utb()$yrke_kod != '***'])
    )
    output$box_utb <- renderText(
      dplyr::n_distinct(vald_x_utb()$utb_kod)
    )
    output$box_bredd <- renderText({
      b <- rekryteringsbredd_median(yrke_x_utb(), input$geografi_val)
      if (is.na(b)) '–' else format(b, decimal.mark = ',')
    })

    # ---- Diagram ---------------------------------------------------------

    output$plot_niva <- renderGirafe({
      d <- fordelning_per_yrke(profil(), 'niva_namn')
      niva_ordning <- profil() |>
        dplyr::distinct(niva_kod, niva_namn) |>
        dplyr::arrange(niva_kod) |>
        dplyr::pull(niva_namn)
      skapa_diagram_fordelning_per_yrke(
        d, niva_ordning,
        farger        = stats::setNames(rd_sekventiell(length(niva_ordning)), niva_ordning),
        markerat_yrke = input$yrke_val,
        underrubrik   = underrubrik(),
        kalla         = KALLA_YRKE_UTB
      )
    })

    output$rubrik_yrke <- renderText({
      namn <- vald_x_utb()$yrke_namn[match(input$yrke_val, vald_x_utb()$yrke_kod)]
      paste0('Vilka utbildningar har ', tolower(namn %||% 'valt yrke'), '?')
    })

    output$rubrik_utb <- renderText({
      namn <- vald_x_utb()$utb_namn[match(input$utb_val, vald_x_utb()$utb_kod)]
      paste0('Vilka yrken har de med utbildning ', namn %||% '', '?')
    })

    detaljdiagram <- function(filter_kol, filter_varde, kat) {
      d <- andel_inom(yrke_x_utb(), input$geografi_val, filter_kol, filter_varde, kat)
      tot <- sum(d$antal[d$geo_niva == 'vald'])
      if (tot < MIN_NAMNARE) {
        return(.girafe_std(
          .tom_plot(paste0('Färre än ', MIN_NAMNARE,
                           ' sysselsätta i urvalet – andelar visas inte')),
          height_svg = 1.5))
      }
      skapa_diagram_andel_inom(d, geo_namn = geo_namn(),
                               underrubrik = paste0(underrubrik(), ' · ',
                                                    format(tot, big.mark = ' '), ' sysselsätta'),
                               kalla = KALLA_YRKE_UTB)
    }

    output$plot_yrke_utb <- renderGirafe({
      shiny::req(input$yrke_val)
      detaljdiagram('yrke_kod', input$yrke_val, 'utb')
    })

    output$plot_utb_yrke <- renderGirafe({
      shiny::req(input$utb_val)
      detaljdiagram('utb_kod', input$utb_val, 'yrke')
    })

    output$plot_matchning <- renderGirafe({
      # Andel matchade räknas bland helt/delvis/inte matchade.
      d <- profil() |>
        dplyr::filter(matchning %in% MATCHNING_GRUPPER) |>
        fordelning_per_yrke('matchning')
      skapa_diagram_fordelning_per_yrke(
        d, MATCHNING_GRUPPER, MATCHNING_FARGER,
        markerat_yrke    = input$yrke_val,
        sortera_kategori = 'Helt matchade',
        underrubrik      = paste0(underrubrik(), ' · sorterat efter andel helt matchade'),
        kalla            = KALLA_YRKE_UTB
      )
    })

    utan_uppgifter <- shiny::reactive({
      shiny::req(input$ar_val, input$geografi_val, input$indelning_val)
      hamta_andel_utan_uppgifter(input$ar_val, input$geografi_val, input$indelning_val)
    }) |> shiny::bindCache(input$ar_val, input$geografi_val, input$indelning_val)

    output$plot_utan_uppgifter <- renderGirafe({
      skapa_diagram_andel_inom(
        utan_uppgifter(), geo_namn = geo_namn(),
        markerad    = input$bransch_val,
        underrubrik = paste0(geo_namn(), ' · år ', input$ar_val),
        kalla       = KALLA_YRKE_UTB
      )
    })

    output$plot_alder <- renderGirafe({
      d <- fordelning_per_yrke(profil(), 'alder')
      kat <- sort(unique(d$kategori))
      skapa_diagram_fordelning_per_yrke(
        d, kat, stats::setNames(rd_sekventiell(length(kat)), kat),
        markerat_yrke    = input$yrke_val,
        sortera_kategori = utils::tail(kat, 1),  # äldsta gruppen: kommande pensionsavgångar
        underrubrik      = paste0(underrubrik(), ' · sorterat efter andel i äldsta gruppen'),
        kalla            = KALLA_YRKE_UTB
      )
    })

    output$plot_kon <- renderGirafe({
      d <- fordelning_per_yrke(profil(), 'kon')
      kat <- c('Kvinna', 'Man')
      farger <- c('Kvinna' = unname(KON_FARGER['Kvinnor']), 'Man' = unname(KON_FARGER['Män']))
      skapa_diagram_fordelning_per_yrke(d, kat, farger,
                                        markerat_yrke    = input$yrke_val,
                                        sortera_kategori = 'Kvinna',
                                        underrubrik      = paste0(underrubrik(), ' · sorterat efter andel kvinnor'),
                                        kalla            = KALLA_YRKE_UTB)
    })
  })
}
