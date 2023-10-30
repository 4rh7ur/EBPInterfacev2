#' The application server-side
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinythemes
#' @import shinyalert
#' @import shinyFile
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  data_to_edit <- dataInputServer("input-1")
  data_edit <- dataEditServer("edit-1",
                              data = data_to_edit
  )
  dataOutputServer("tab",
                   data = data_edit
  )
  mod_tratamento_server("tratamento_1")
  mod_status_server("status_1")
  mod_carga_server("carga_1")
  mod_edicao_server("edicao_1")
  mod_mod_valid_server("mod_valid_1")
}
