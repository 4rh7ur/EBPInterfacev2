#' edicao UI Function
#' @import DataEditR
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_edicao_ui <- function(id){
  ns <- NS(id)
  tagList(
    DataEditR::dataInputUI("input-1"),
    DataEditR::dataOutputUI("tab"),
    DataEditR::dataEditUI("edit-1")
  )
}

#' edicao Server Functions
#'
#' @noRd
mod_edicao_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_to_edit <- DataEditR::dataInputServer("input-1")
    data_edit <- DataEditR::dataEditServer("edit-1",
                                data = data_to_edit
    )
    DataEditR::dataOutputServer("tab",
                     data = data_edit
    )

  })
}

## To be copied in the UI
# mod_edicao_ui("edicao_1")

## To be copied in the server
# mod_edicao_server("edicao_1")
