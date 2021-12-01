#' carga UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_carga_ui <- function(id){
  ns <- NS(id)
  tagList(
  shiny::fluidRow(shiny::column(
  shiny::fileInput(ns("carga_final"), "Indique o Diretório do Dataset a Ser Incluído",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
  shiny::actionButton("carga1", "Executar Carga Incremental"),
  shiny::actionButton("carga1", "Executar Carga Completa"), width = 10))

  )
}

#' carga Server Functions
#'
#' @noRd
mod_carga_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    myData5 <- reactive({
      inFile <- input$file
      if (is.null(inFile)) return(NULL)
      #data

      return(data)
    })

  })
}

## To be copied in the UI
# mod_carga_ui("carga_1")

## To be copied in the server
# mod_carga_server("carga_1")
