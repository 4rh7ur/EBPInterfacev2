library(shiny)
library(tidyverse)
library(DataEditR)
library(shinyFiles)
library(ETLEBP)
ui <- fluidPage(tabsetPanel(tabPanel("Status",
                                     tableOutput("status")), #fim tabpanel1
                            tabPanel("Estação de Tratamento", id = "a2",
                                     fluidRow(
                                       column(selectInput("id1", "Selecione a Fonte dos Dados que Receber? o Tratamento",
                                                          choices = c("Fonte ANEEL", "Fonte ANP",
                                                                      "Fonte BNDES", "Fonte CNEN",
                                                                      "Fonte CNPQ", "Fonte FAPESP",
                                                                      "Fonte FINEP", "Fonte FNDCT")), width = 10)), #Fim da fluidrow1
                                     fluidRow(conditionalPanel(condition = "input.id1 == 'Fonte ANEEL'",
                                                               fileInput("file1", "Indique o diretório de PD Busca Textual",
                                                                         multiple = FALSE,
                                                                         accept = c("text/csv",
                                                                                    "text/comma-separated-values,text/plain",
                                                                                    ".csv")),
                                                               fileInput("file11", "Indique o Diretório de 5.PD RF EQUIPE",
                                                                         multiple = FALSE,
                                                                         accept = c("text/csv",
                                                                                    "text/comma-separated-values,text/plain",
                                                                                    ".csv")),
                                                               DT::dataTableOutput('contents'),
                                                               downloadButton("download", "Baixar Dataset"),
                                                               width = 10),

                                              conditionalPanel(condition = "input.id1 == 'Fonte ANP'",
                                                               fileInput("file2", "Indique o Diretório de projetos-rt-3-2015",
                                                                         multiple = FALSE,
                                                                         accept = c("text/csv",
                                                                                    "text/comma-separated-values,text/plain",
                                                                                    ".csv")),
                                                               fileInput("file22", "Indique o diretório de anp_agregados_declarados",
                                                                         multiple = FALSE,
                                                                         accept = c("text/csv",
                                                                                    "text/comma-separated-values,text/plain",
                                                                                    ".csv")),
                                                               DT::dataTableOutput('contents2'),
                                                               downloadButton("download2", "Baixar Dataset"),
                                                               width = 10),

                                              conditionalPanel(condition = "input.id1 == 'Fonte BNDES'",
                                                               fileInput("file3", "Indique o Diretório de naoautomaticas",
                                                                         multiple = FALSE,
                                                                         accept = c("text/csv",
                                                                                    "text/comma-separated-values,text/plain",
                                                                                    ".csv")),

                                                               DT::dataTableOutput('contents3'),
                                                               downloadButton("download3", "Baixar Dataset"),
                                                               width = 10),

                                              conditionalPanel(condition = "input.id1 == 'Fonte CNEN'",
                                                               fileInput("file4", "Indique o Diretório de Projeto CNEN_Plataforma Inova-E",
                                                                         multiple = FALSE,
                                                                         accept = c("text/csv",
                                                                                    "text/comma-separated-values,text/plain",
                                                                                    ".csv")),

                                                               DT::dataTableOutput('contents4'),
                                                               downloadButton("download4", "Baixar Dataset"),
                                                               width = 10),

                                              conditionalPanel(condition = "input.id1 == 'Fonte FINEP'",
                                                               fileInput("file5", "Indique o Diretório de 14_09_2021_Liberacoes",
                                                                         multiple = FALSE,
                                                                         accept = c("text/csv",
                                                                                    "text/comma-separated-values,text/plain",
                                                                                    ".csv")),

                                                               DT::dataTableOutput('contents5'),
                                                               downloadButton("download5", "Baixar Dataset"),
                                                               width = 10),
                                              )#Fim da fluidrow2

                            ),#fim tabpanel2
                            tabPanel("Estação de Edição",
                                     dataInputUI("input-1"),
                                     dataOutputUI("tab"),
                                     dataEditUI("edit-1"))#fim tabpanel3
),#fim tabsetpanel
tags$head(tags$script(src = "message-handler.js")))#fim ui


server <- function(input, output, session) {
  #Visualization Status

  df<-data.frame("Fonte" = c("Fonte ANEEL", "Fonte ANP",
                             "Fonte BNDES", "Fonte CNEN",
                             "Fonte CNPQ", "Fonte FAPESP",
                             "Fonte FINEP", "Fonte FNDCT"),
                 "Status" = c("Não Executado","Não Executado",
                              "Não Executado","Não Executado",
                              "Não Executado","Não Executado",
                              "Não Executado","Não Executado"))

  output$status <- renderTable(df)
  #Action button
  observeEvent(input$carga1, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Op??o de Carga Incremental Selecionada')
  })

  observeEvent(input$carga2, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Op??o de Carga Completa Selecionada')
  })
  #Dataedit
  data_to_edit <- dataInputServer("input-1")
  data_edit <- dataEditServer("edit-1",
                              data = data_to_edit
  )
  dataOutputServer("tab",
                   data = data_edit
  )
  #declarando a função


  #Importando dados

  options(shiny.maxRequestSize=80*1024^3) # Maximum upload size allowed, in bytes
#Aneel
  myData <- reactive({
    inFile <- input$file1
    #if (is.null(inFile)) return(NULL)
    inFile11 <- input$file11
    #if (is.null(inFile)) return(NULL)
    #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
    data <- ETLEBP::cria_base_intermediaria_aneel(origem_processos = inFile$datapath,
                                                  origem_equipes = inFile11$datapath)

    return(data)
  })
#Renderizar Dataset
  output$contents <- DT::renderDataTable({
    setcolorder(myData(), input$rank_list_1)
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
    #if (is.null(inFile)) return(NULL)
    inFile22 <- input$file22
    #if (is.null(inFile)) return(NULL)
    #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
    data <- ETLEBP::cria_base_intermediaria_anp(origem_processos = inFile2$datapath,
                                                  origem_enriquecimento = inFile22$datapath)

    return(data)
  })
  #Renderizar Dataset
  output$contents2 <- DT::renderDataTable({
    setcolorder(myData2(), input$rank_list_1)
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
    #if (is.null(inFile)) return(NULL)
    #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
    data <- ETLEBP::cria_base_intermediaria_bndes(origem_processos = inFile3$datapath)

    return(data)
  })
  #Renderizar Dataset
  output$contents3 <- DT::renderDataTable({
    setcolorder(myData3(), input$rank_list_1)
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
    #if (is.null(inFile)) return(NULL)
    #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
    data <- ETLEBP::cria_base_intermediaria_cnen(origem_processos = inFile4$datapath)

    return(data)
  })
  #Renderizar Dataset
  output$contents4 <- DT::renderDataTable({
    setcolorder(myData4(), input$rank_list_1)
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
  #CNEN
  myData5 <- reactive({
    inFile5 <- input$file5
    #if (is.null(inFile)) return(NULL)
    #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
    data <- ETLEBP::cria_base_intermediaria_finep(origem_processos = inFile5$datapath)

    return(data)
  })
  #Renderizar Dataset
  output$contents5 <- DT::renderDataTable({
    setcolorder(myData5(), input$rank_list_1)
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
}

shinyApp(ui, server)


