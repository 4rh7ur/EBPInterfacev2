#' tratamento UI Function
#' @import shiny
#' @import data.table
#' @import sortable
#' @import ETLEBP
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tratamento_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(selectInput("id1", "Selecione a Fonte dos Dados que Receberá o Tratamento",
                         choices = c("Fonte ANEEL", "Fonte ANP",
                                     "Fonte BNDES", "Fonte CNEN",
                                     "Fonte CNPQ", "Fonte FAPESP",
                                     "Fonte FINEP", "Fonte FNDCT")), width = 10)), #Fim da fluidrow1
    fluidRow(conditionalPanel(condition = "input.id1 == 'Fonte ANEEL'",
                              fileInput(ns("file1"), "Indique o diretório de PD Busca Textual",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              fileInput(ns("file11"), "Indique o diretório de 5.PD RF EQUIPE",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),

                              downloadButton(ns("download"), "Executar Tratamento e Baixar Dataset"),
                              width = 10),

             conditionalPanel(condition = "input.id1 == 'Fonte ANP'",
                              fileInput(ns("file2"), "Indique o diretório de projetos-rt-3-2015",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              fileInput(ns("file22"), "Indique o diretório de anp_agregados_declarados",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),

                              downloadButton(ns("download2"), "Executar Tratamento e Baixar Dataset"),
                              width = 10),

             conditionalPanel(condition = "input.id1 == 'Fonte BNDES'",
                              fileInput(ns("file3"), "Indique o diretório de naoautomaticas",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),


                              downloadButton(ns("download3"), "Executar Tratamento e Baixar Dataset"),
                              width = 10),

             conditionalPanel(condition = "input.id1 == 'Fonte CNEN'",
                              fileInput(ns("file4"), "Indique o diretório de Projeto CNEN_Plataforma Inova-E",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),


                              downloadButton(ns("download4"), "Executar Tratamento e Baixar Dataset"),
                              width = 10),

             conditionalPanel(condition = "input.id1 == 'Fonte FINEP'",
                              fileInput(ns("file5"), "Indique o diretório de 14_09_2021_Liberacoes",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),

                                                            downloadButton(ns("download5"), "Executar Tratamento e Baixar Dataset"),
                              width = 10),
    )#Fim da fluidrow2



  )#fim taglist
}

#' tratamento Server Functions
#'
#' @noRd
mod_tratamento_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #Importando dados

    options(shiny.maxRequestSize=80*1024^3) # Maximum upload size allowed, in bytes
    #Aneel
    myData <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      inFile11 <- input$file11
      if (is.null(inFile11)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_aneel(origem_processos = inFile$datapath,
                                                    origem_equipes = inFile11$datapath)

      return(data)
    })

    #Fazer o Download
    output$download <- downloadHandler(
      filename = function() {
        paste(myData(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData(), file, row.names = FALSE)
      }
    )

    #ANP
    myData2 <- reactive({
      inFile2 <- input$file2
      if (is.null(inFile2)) return(NULL)
      inFile22 <- input$file22
      if (is.null(inFile22)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_anp(origem_processos = inFile2$datapath,
                                                  origem_enriquecimento = inFile22$datapath)

      return(data)
    })

    #Fazer o Download
    output$download2 <- downloadHandler(
      filename = function() {
        paste(myData2(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData2(), file, row.names = FALSE)
      }
    )
    #BNDES
    myData3 <- reactive({
      inFile3 <- input$file3
      if (is.null(inFile3)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_bndes(origem_processos = inFile3$datapath)

      return(data)
    })

    #Fazer o Download
    output$download3 <- downloadHandler(
      filename = function() {
        paste(myData3(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData3(), file, row.names = FALSE)
      }
    )
    #CNEN
    myData4 <- reactive({
      inFile4 <- input$file4
      if (is.null(inFile4)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_cnen(origem_processos = inFile4$datapath)

      return(data)
    })

    #Fazer o Download
    output$download4 <- downloadHandler(
      filename = function() {
        paste(myData4(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData4(), file, row.names = FALSE)
      }
    )
    #FINEP
    myData5 <- reactive({
      inFile5 <- input$file5
      if (is.null(inFile5)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_finep(origem_processos = inFile5$datapath)

      return(data)
    })

    #Fazer o Download
    output$download5 <- downloadHandler(
      filename = function() {
        paste(myData5(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData5(), file, row.names = FALSE)
      }
    )


  })
}

## To be copied in the UI
# mod_tratamento_ui("tratamento_1")

## To be copied in the server
# mod_tratamento_server("tratamento_1")
