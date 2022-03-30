#' carga UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import RSQLite
#' @importFrom shiny NS tagList
mod_carga_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(shiny::column(shinyalert::useShinyalert(),
                                  shiny::fileInput(ns("carga_final"), "Indique o Diretório do Dataset a Ser Incluído",
                                                   multiple = FALSE,
                                                   accept = c("text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                  shiny::fileInput(ns("sqlite"), "Indique o Diretório do SQLite"),
                                  shiny::actionButton(ns("carga1"), "Executar Carregamento Completo no SQLite"),
                                  shiny::actionButton(ns("carga2"), "Executar Carregamento Incremental no SQLite"),
                                  shiny::actionButton(ns("carga3"), "Executar Carga Incremental ANP no SQLite"),
                                  width = 10))


  )
}

#' carga Server Functions
#'
#' @noRd
mod_carga_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    myData <- reactive({
      inFile <- input$carga_final
      if (is.null(inFile)) return(NULL)
      data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))

      return(data)
    })

    data<- reactive({
      data<- read.csv(input$carga_final$datapath)
    })

 myData_sqlite <- reactive({

   filesqlite<- input$sqlite
   if (is.null(filesqlite)) return(NULL)
   filename <- here::here(filesqlite$datapath)
 })




    observeEvent(input$carga1, { executa_carga_completa(data(), filename)


    })


    #Mandar msg p usuário
    observeEvent(input$carga1, {
      shinyalert::shinyalert(title = "OK!",
                             text = "Carregado para o SQLite com Sucesso.",
                             type = "success")

    })

    #------------------------
    #Carga incremental

    data<- reactive({
      data<- read.csv(input$carga_final$datapath)
    })




    observeEvent(input$carga2, {ETLEBP::executa_carga_incremental(data(), filename)
    })


    #Mandar msg p usuário
    observeEvent(input$carga2, {
      shinyalert::shinyalert(title = "OK!",
                             text = "Carregado para o SQLite com Sucesso.",
                             type = "success")

    })
 ## Carga Incremental ANP ------------------------------------------------------------------

    data<- reactive({
      data<- read.csv(input$carga_final$datapath)
    })



    observeEvent(input$carga3, {ETLEBP::executa_carga_gc(data(), filename)
    })


    #Mandar msg p usuário
    observeEvent(input$carga3, {
      shinyalert::shinyalert(title = "OK!",
                             text = "Carregado para o SQLite com Sucesso.",
                             type = "success")

    })


  })
}

## To be copied in the UI
# mod_carga_ui("carga_1")

## To be copied in the server
# mod_carga_server("carga_1")
