# Enkel platshållarmodul som visas i flikar som ännu inte är byggda.
# Ersätts flik för flik med en riktig mod_<flik>.R allteftersom vi
# arbetar oss igenom strukturen.

mod_kommer_snart_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "d-flex flex-column align-items-center justify-content-center text-muted",
    style = "min-height: 400px;",
    bsicons::bs_icon("hammer", size = "2.5rem"),
    h4(class = "mt-3", "Den här fliken byggs i en kommande iteration.")
  )
}

mod_kommer_snart_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
