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

    # ---- Startmeddelande: appen är under utveckling -----------------------
    # Dyker upp vid start, ligger kvar en lång stund så att alla hinner läsa
    # den, och går även att stänga direkt med krysset.
    tags$div(
      id = 'rd-dev-notice', class = 'rd-dev-notice',
      tags$div(
        class = 'rd-dev-notice__kort', role = 'status',
        tags$button(
          type = 'button', class = 'rd-dev-notice__stang',
          onclick = "document.getElementById('rd-dev-notice').classList.add('rd-dev-notice--dold')",
          `aria-label` = 'Stäng meddelandet', HTML('&times;')
        ),
        h3('Appen är under utveckling'),
        p('Den här applikationen byggs och utvecklas löpande. Det kommer att ',
          'ske en hel del förändringar framöver, så nästa gång du besöker ',
          'sidan kan den se annorlunda ut.'),
        p('Har du frågor eller funderingar? Hör gärna av dig till ',
          tags$a(href = 'mailto:samhallsanalys@regiondalarna.se',
                 'samhallsanalys@regiondalarna.se'), '.')
      ),
      tags$script(HTML(
        "setTimeout(function() {
           var el = document.getElementById('rd-dev-notice');
           if (el) el.classList.add('rd-dev-notice--dold');
         }, 25000);"
      ))
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
        tabPanel('Behov & rekrytering',     mod_behov_ui('behov')),
        tabPanel('Rörlighet & hälsa',       mod_flik_placeholder_ui('rorlighet_halsa', 'Rörlighet & hälsa')),
        tabPanel('Prognos',                 mod_prognos_ui('prognos')),

        tabPanel(
          'Om rapporten',
          div(class = 'rd-card',
              h2('Om rapporten'),
              p('Den här applikationen visar branschstatistik för Dalarna, ',
                'utifrån inspel från branschorganisationerna. Flikarna ',
                'Behov & rekrytering och Rörlighet & hälsa tillkommer efter hand.'),
              p('Appen är under utveckling och byggs vidare på löpande, så utseende ',
                'och innehåll kan komma att ändras. Har du frågor eller funderingar ',
                'är du välkommen att höra av dig till ',
                tags$a(href = 'mailto:samhallsanalys@regiondalarna.se',
                       'samhallsanalys@regiondalarna.se'), '.'),
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
