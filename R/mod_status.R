#' status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @import RSQlite
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_status_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput("fonte", "Selecione a Fonte dos Dados",
                choices = c(
                  "Fonte ANEEL",
                  "Fonte ANP",
                  "Fonte BNDES",
                  "Fonte CNEN",
                  "Fonte CNPQ",
                  "Fonte FAPESP",
                  "Fonte FINEP",
                  "Fonte FNDCT"
                ),selected = "Fonte ANEEL"),
    selectInput("status2", "Selecione o Status",
                choices = c(
                  "Não Executada",
                  "Tratada",
                  "Em Edição",
                  "Validada",
                  "Carregada no SQlite"
                )),
    actionButton("ns(atualizar)", "atualizar status")

  )
}

#' status Server Functions
#'
#' @noRd
mod_status_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    df <- data.frame(
      "Fonte" = c(
        "Fonte ANEEL",
        "Fonte ANP",
        "Fonte BNDES",
        "Fonte CNEN",
        "Fonte CNPQ",
        "Fonte FAPESP",
        "Fonte FINEP",
        "Fonte FNDCT"
      ),
      "Status" = c(
        "Em Edição",
        "Não Executado",
        "Não Executado",
        "Não Executado",
        "Não Executado",
        "Não Executado",
        "Não Executado",
        "Não Executado"
      )
    )

    globals <- reactiveValues(
      mydf = df
    )


    observe({})

    output$status <- renderTable(df)
  })
}

## To be copied in the UI
# mod_status_ui("status_1")

## To be copied in the server
# mod_status_server("status_1")
