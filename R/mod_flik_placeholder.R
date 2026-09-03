# ============================================================
#  mod_flik_placeholder.R – platshållarmodul för flikar som inte
#  är byggda än, i linje med mod_skolform_placeholder.R i utbildningsappen.
# ============================================================

mod_flik_placeholder_ui <- function(id, flik_namn) {
  ns <- NS(id)

  div(class = 'rd-card',
      h2(flik_namn),
      p('Den h\u00e4r fliken byggs i en kommande iteration.')
  )
}

mod_flik_placeholder_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
