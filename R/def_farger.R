# ============================================================
#  def_farger.R – färgprofil, Region Dalarna
#
#  OBS: rekonstruerad utifrån CSS-variablerna i regiondalarna_ruf.css/
#  app.css (RD_HOVER_CSS/RD_SELECT_CSS är gissningar på interaktions-css
#  för ggiraph, byggda i samma stil som .tippy-box[data-theme~='rd']).
#  Ersätt med den riktiga def_farger.R om värdena skiljer sig.
# ============================================================

RD_PRIMARY      <- "#158daf"
RD_PRIMARY_DARK <- "#0f7090"
RD_ACCENT       <- "#54a1bd"

RD_TEXT       <- "#212529"
RD_TEXT_MUTED <- "#6c757d"

KON_FARGER <- c("Kvinnor" = "#e2a855", "M\u00e4n" = "#459079")

# ggiraph-interaktion: samma "tippy"-look som app.css (tippy-box[data-theme~='rd'])
RD_TOOLTIP_CSS <- paste0(
  "background-color:", RD_TEXT, "; color:#fff; font-family:Poppins,Arial,sans-serif;",
  "font-size:12px; padding:6px 9px; border-radius:4px; line-height:1.35;"
)
RD_HOVER_CSS  <- paste0("stroke:", RD_PRIMARY_DARK, ";stroke-width:1.5px;")
RD_SELECT_CSS <- paste0("stroke:", RD_PRIMARY, ";stroke-width:2px;fill-opacity:1;")
