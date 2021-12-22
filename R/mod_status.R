#' status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import RSQLite
#' @importFrom shiny NS tagList
mod_status_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("fonte"), "Selecione a Fonte dos Dados",
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
    selectInput(ns("status2"), "Selecione o Status",
                choices = c(
                  "Não Executada",
                  "Tratada",
                  "Em Edição",
                  "Validada",
                  "Carregada no SQlite"
                ), selected = "Tratada"),
    #dateInput(inputId = "SessionDate", label = "Data", format = "dd-mm-yyyy"),
    actionButton(ns("atualizar"), "atualizar status"),
   # textOutput(ns("result")),
    dataTableOutput(ns("TheData"))

  )
}

#' status Server Functions
#'
#' @noRd
mod_status_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
df<- data.frame(fonte=c("Fonte ANEEL",
                 "Fonte ANP",
                 "Fonte BNDES",
                 "Fonte CNEN",
                 "Fonte CNPQ",
                 "Fonte FAPESP",
                 "Fonte FINEP",
                 "Fonte FNDCT"),
                status = c("Não Executado",
                           "Não Executado",
                           "Não Executado",
                           "Não Executado",
                           "Não Executado",
                           "Não Executado",
                           "Não Executado",
                           "Não Executado"))

#output$result <- renderText({
#  paste("You chose", input$status2)
#})
    #Corrigir algum bug
observeEvent(input$atualizar, {
  status <- DBI::dbConnect(RSQLite::SQLite(), dbname="status_sqlite")

t1<- input$status2
t2<- input$fonte

  query<-sprintf("UPDATE status SET status = '%s' where Fonte = '%s'",
                 t1,t2)

  DBI::dbExecute(status, query)

  t3<- as.Date(Sys.Date())

  query2<-sprintf("UPDATE status SET data = '%s' where Fonte = '%s'",
                  t3,t2)

  DBI::dbExecute(status, query2)

  ndf <- DBI::dbReadTable(status,"status")
  output$TheData <- renderDataTable({
    ndf
  })

})



  })
}

## To be copied in the UI
# mod_status_ui("status_1")

## To be copied in the server
# mod_status_server("status_1")
