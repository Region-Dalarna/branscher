shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'app.css'),
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
        tags$img(src = 'logo_liggande_fri_vit.png', alt = 'Region Dalarna'),
        tags$span('Samhällsanalys')
      )
    ),

    # ---- N1: Flikar (yttre tabsetPanel), hela bredden ---------------------
    div(
      style = 'padding: 8px 24px 24px;',
      tabsetPanel(
        id = 'flik',

        tabPanel('Översikt', mod_oversikt_ui('oversikt')),
        tabPanel('Sysselsättning & yrken',  mod_flik_placeholder_ui('sysselsattning',  'Sysselsättning & yrken')),
        tabPanel('Demografi',               mod_flik_placeholder_ui('demografi',       'Demografi')),
        tabPanel('Utbildning',              mod_flik_placeholder_ui('utbildning',      'Utbildning')),
        tabPanel('Behov & rekrytering',     mod_flik_placeholder_ui('behov',           'Behov & rekrytering')),
        tabPanel('Rörlighet & hälsa',       mod_flik_placeholder_ui('rorlighet_halsa', 'Rörlighet & hälsa')),
        tabPanel('Prognos',                 mod_flik_placeholder_ui('prognos',         'Prognos')),

        tabPanel(
          'Om rapporten',
          div(class = 'rd-card',
              h2('Om rapporten'),
              p('Den här applikationen visar branschstatistik för Dalarna, ',
                'utifrån inspel från branschorganisationerna. I nuvarande ',
                'version är Översikt inlagd; övriga flikar tillkommer efter hand.'),
              div(class = 'rd-info',
                  tags$strong('Källa: '),
                  'SCB (RAMS) m.fl., bearbetat av Region Dalarna.')
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
