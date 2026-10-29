# ============================================================
#  def_lankning.R – djuplänkning (URL → flik/diagramval)
#
#  Query-parametern "flik" i URL:en väljer flik vid sidladdning, t.ex.
#  https://.../?flik=oversikt. Nycklarna här är korta, URL-vänliga
#  slugs; värdena är flikarnas riktiga namn (måste matcha tabPanel-
#  titlarna i ui.R ordagrant).
#
#  Enskilda flikar kan läsa fler query-parametrar för att återskapa ett
#  visst diagramval -- se R/mod_oversikt.R ("lank"-argumentet till
#  mod_oversikt_server()) för ett exempel att följa i andra flikar.
# ============================================================

FLIK_SLUGS <- c(
  oversikt   = 'Översikt',
  utbildning = 'Utbildning & yrken',
  matchning  = 'Matchning',
  demografi  = 'Demografi',
  behov      = 'Behov & rekrytering',
  rorlighet  = 'Rörlighet & hälsa',
  prognos    = 'Prognos',
  om         = 'Om rapporten'
)
