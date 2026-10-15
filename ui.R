shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = www_version('regiondalarna_ruf.css')),
      tags$link(rel = 'stylesheet', type = 'text/css', href = www_version('app.css')),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'tippy.css'),
      tags$script(src = 'popper.min.js'),
      tags$script(src = 'tippy-bundle.umd.min.js'),
      tags$script(src = 'tooltips.js')
    ),

    # ---- Header (full bredd via app.css) ---------------------------------
    tags$div(
      class = 'rd-header',
      tags$div(class = 'rd-header__title', 'Branschstatistik Dalarna'),
      tags$a(
        class  = 'rd-header__right',
        href   = 'https://www.regiondalarna.se',
        target = '_blank',
        tags$img(src = 'logo_liggande_fri_vit.png', alt = 'Region Dalarna')
      )
    ),

    # ---- N1: Flikar (yttre tabsetPanel), hela bredden ---------------------
    div(
      style = 'padding: 8px 24px 24px;',
      tabsetPanel(
        id = 'flik',

        tabPanel('Översikt', mod_oversikt_ui('oversikt')),
        tabPanel('Utbildning & yrken',      mod_utbildning_yrken_ui('utbildning_yrken')),
        tabPanel('Matchning',               mod_matchning_ui('matchning')),
        tabPanel('Demografi',               mod_demografi_ui('demografi')),
        tabPanel('Behov & rekrytering',     mod_flik_placeholder_ui('behov',           'Behov & rekrytering')),
        tabPanel('Rörlighet & hälsa',       mod_flik_placeholder_ui('rorlighet_halsa', 'Rörlighet & hälsa')),
        tabPanel('Prognos',                 mod_prognos_ui('prognos')),

        tabPanel(
          'Om rapporten',
          div(class = 'rd-card',
              h2('Om rapporten'),
              p('Den här applikationen visar branschstatistik för Dalarna, ',
                'utifrån inspel från branschorganisationerna. Flikarna ',
                'Behov & rekrytering och Rörlighet & hälsa tillkommer efter hand.'),
              h3('Flikar och källor'),
              tags$ul(
                tags$li(tags$strong('Översikt: '),
                        'sysselsatta och etablerade per bransch i Dalarnas kommuner, länet och riket. ',
                        'Källa: SCB (RAMS).'),
                tags$li(tags$strong('Utbildning & yrken, Matchning, Demografi: '),
                        'sysselsatta efter yrke, utbildning, matchning, ålder, kön och bakgrund. ',
                        'Källa: SCB (Yrkesregistret, Utbildningsregistret).'),
                tags$li(tags$strong('Prognos: '),
                        'förvärvsarbetande per näringsgren och arbetskraften 2023–2040, Dalarnas län ',
                        'och riket. Källa: SCB, Regionala trender och prognoser 2025 (RegTop), ',
                        'publicerad 2026-05-25. RegTop använder egna näringsgrenar och finns bara ',
                        'för län och riket.')
              ),
              div(class = 'rd-info',
                  tags$strong('Sekretess: '),
                  'Antal under 4 visas som “färre än 4”. Andelar för yrken, utbildningar ',
                  'och branscher visas bara när underlaget är minst 20 sysselsatta.'),
              p(class = 'rd-subtitle', style = 'margin-top: 12px;',
                'Bearbetning: Samhällsanalys, Region Dalarna.')
          )
        )
      )
    ),

    # ---- Footer (full bredd via app.css) ----------------------------------
    tags$div(
      class = 'rd-footer',
      'Samhällsanalys, Region Dalarna · ',
      tags$a(
        href = 'mailto:samhallsanalys@regiondalarna.se',
        'samhallsanalys@regiondalarna.se'
      )
    )
  )
)
