#' status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_status_ui <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(ns("status"))

  )
}

#' status Server Functions
#'
#' @noRd
mod_status_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    df<-data.frame("Fonte" = c("Fonte 1","Fonte 2",
                               "Fonte 3","Fonte 4",
                               "Fonte 5","Fonte 6",
                               "Fonte 7","Fonte 8"),
                   "Status" = c("Não Executado","Não Executado",
                                "Não Executado","Não Executado",
                                "Não Executado","Não Executado",
                                "Não Executado","Não Executado"))

    output$status <- renderTable(df)
  })
}

## To be copied in the UI
# mod_status_ui("status_1")

## To be copied in the server
# mod_status_server("status_1")
