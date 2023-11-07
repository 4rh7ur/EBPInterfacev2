#' carga UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import RSQLite
#' @import shinyFiles
#' @import fs
#' @importFrom shiny NS tagList

mod_carga_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(shiny::column(shinyalert::useShinyalert(),
                                  # shiny::fileInput(ns("carga_final"), "Indique o Diretório do Dataset a Ser Incluído",
                                  #                  multiple = FALSE,
                                  #                  accept = c("text/csv",
                                  #                             "text/comma-separated-values,text/plain",
                                  #                             ".csv")),
                                  h5(HTML(paste0("<b>","1.Indique o Diretório do Dataset a Ser Incluído","</b>"))),
                                  shinyFiles::shinyFilesButton(ns("carga_final"), "Buscar", "Indique o Diretório do Dataset a Ser Incluído", multiple = F, viewtype = "detail"),
                                  h5(HTML(paste0("<b>","2.Indique o Diretório da Base SQLite","</b>"))),
                                  shinyFiles::shinyFilesButton(ns("sqlite"), "Buscar", "Indique o Diretório da Base SQLite", multiple = FALSE, viewtype = "detail"),
                                  #shiny::fileInput(ns("sqlite"), "Indique o Diretório da Base SQLite"),
                                  hr(),
                                  shiny::actionButton(ns("carga1"), "Executar Carregamento Completo no SQLite"),
                                  shiny::actionButton(ns("carga2"), "Executar Carregamento Incremental no SQLite"),
                                  width = 10))


  )
}

#' carga Server Functions
#'
#' @noRd
mod_carga_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
    # by setting `allowDirCreate = FALSE` a user will not be able to create a new directory
    shinyFiles::shinyFileChoose(input, "carga_final", roots = volumes, session = session)
    shinyFiles::shinyFileChoose(input, "sqlite", roots = volumes, session = session)

    sel_path <- reactive({return(print(data.frame(shinyFiles::parseFilePaths(volumes, input$sqlite))$datapath))})


    myData <- reactive({
      inFile <- input$carga_final
      if (is.null(inFile)) return(NULL)
      data<- read.csv(data.frame(shinyFiles::parseFilePaths(volumes, inFile))$datapath)
    })

    #------------------------
    #Carga completa

    observeEvent(input$carga1,
                 {ETLEBP::executa_carga_completa(myData(), sel_path())
                 })

    #Mandar msg p usuário
    observeEvent(input$carga1, {
      shinyalert::shinyalert(title = "OK!",
                             text = "Carregado para o SQLite com Sucesso.",
                             type = "success")

    })

    #------------------------
    #Carga incremental

    observeEvent(input$carga2,
                 {ETLEBP::executa_carga_incremental(myData(), sel_path())

    })

    #Mandar msg p usuário
    observeEvent(input$carga2, {
      shinyalert::shinyalert(title = "OK!",
                             text = "Carregado para o SQLite com Sucesso.",
                             type = "success")

    })
  })
}


