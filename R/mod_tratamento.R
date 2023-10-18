#' tratamento UI Function
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
    #Identificar o Tipo de Carga
    fluidRow(column(
      selectInput(
        "id0",
        "1. Selecione o Tipo de Carga",
        choices = c("Carga Completa", "Carga Incremental")
      ),
      fileInput(ns("file_sqlite"), "Indique o diretório da base SQLite"),
      width = 10
    )),

    #Iniciando Carga Completa
    fluidRow(column(
      conditionalPanel(
        condition = "input.id0 == 'Carga Completa'",
        selectInput(
          "id1",
          "2. Selecione a Fonte dos Dados que Receberá o Tratamento",
          choices = c(
            "Fonte ANEEL",
            "Fonte ANP",
            "Fonte BNDES",
            "Fonte CNEN",
            "Fonte CNPQ",
            "Fonte FAPESP",
            "Fonte FINEP"
          )
        ),
        fluidRow(
          conditionalPanel(
            condition = "input.id1 == 'Fonte ANEEL'",
            fileInput(
              ns("file1"),
              "3. Indique o diretório da base primária da ANEEL",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
            ),

            downloadButton(ns("download"), "Executar Tratamento e Baixar Dataset"),
            width = 10
          ),

          conditionalPanel(
            condition = "input.id1 == 'Fonte ANP'",
            fileInput(
              ns("file2"),
              "3. Indique o diretório da base primária da ANP",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
            ),

            downloadButton(ns("download2"), "Executar Tratamento e Baixar Dataset"),
            width = 10
          ),

          conditionalPanel(
            condition = "input.id1 == 'Fonte BNDES'",
            fileInput(
              ns("file3"),
              "3. Indique o diretório da base primária do BNDES",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".ods",
                         ".xlsx",
                         ".xls")
            ),


            downloadButton(ns("download3"), "Executar Tratamento e Baixar Dataset"),
            width = 10
          ),

          conditionalPanel(
            condition = "input.id1 == 'Fonte CNEN'",
            fileInput(
              ns("file4"),
              "3. Indique o diretório da base primária da CNEN",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".xlsx",
                         ".xls",
                         ".ods")
            ),


            downloadButton(ns("download4"), "Executar Tratamento e Baixar Dataset"),
            width = 10
          ),

          conditionalPanel(
            condition = "input.id1 == 'Fonte FINEP'",
            fileInput(
              ns("file5"),
              "3. Indique o diretório da base primária da FINEP",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".ods",
                         ".xlsx",
                         ".xls")
            ),

            downloadButton(ns("download5"), "Executar Tratamento e Baixar Dataset"),
            width = 10
          ),
          conditionalPanel(
            condition = "input.id1 == 'Fonte FAPESP'",
            fileInput(
              ns("file6"),
              "3. Indique o diretório da base primária da FAPESP",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".xlsx",
                         ".xls",
                         ".ods")
            ),

            downloadButton(ns("download6"), "Executar Tratamento e Baixar Dataset"),
            width = 10
          ),

          conditionalPanel(
            condition = "input.id1 == 'Fonte CNPQ'",
            fileInput(
              ns("file7"),
              "3. Indique o diretório da base primária do CNPq",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".xlsx",
                         ".xls",
                         ".ods")
            ),

            downloadButton(ns("download7"), "Executar Tratamento e Baixar Dataset"),
            width = 10
          )
        )
      ),#fim conditional panel

      width = 10
    )#fim column
    ), #fim fluidrow

        #Iniciando Carga Incremental
    fluidRow(
      column(
        conditionalPanel(
          condition = "input.id0 == 'Carga Incremental'",
          selectInput(
            "id2",
            "2. Selecione a Fonte dos Dados que Receberá o Tratamento",
            choices = c(
              "Fonte ANEEL",
              "Fonte ANP",
              "Fonte BNDES",
              "Fonte CNEN",
              "Fonte CNPQ",
              "Fonte FAPESP",
              "Fonte FINEP",
              "Fonte FNDCT"
            )
          ),

          fluidRow(
            conditionalPanel(
              condition = "input.id2 == 'Fonte ANEEL'",
              fileInput(
                ns("i.file1"),
                "3. Indique o diretório da base primária da ANEEL",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              ),

              downloadButton(ns("i.download"), "Executar Tratamento e Baixar Dataset"),
              width = 10
            ),
            #fim conditional Panel

            conditionalPanel(
              condition = "input.id2 == 'Fonte ANP'",
              fileInput(
                ns("i.file2"),
                "3. Indique o diretório da base primária da ANP",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              ),

              downloadButton(ns("i.download2"), "Executar Tratamento e Baixar Dataset")
            ),
            #fim conditional Panel
            conditionalPanel(
              condition = "input.id2 == 'Fonte BNDES'",
              fileInput(
                ns("i.file3"),
                "3. Indique o diretório da base primária do BNDES",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xlsx",
                           ".xls",
                           ".ods")
              ),


              downloadButton(ns("i.download3"), "Executar Tratamento e Baixar Dataset"),
              width = 10
            ),
            #fim conditional panel

            conditionalPanel(
              condition = "input.id2 == 'Fonte CNEN'",
              fileInput(
                ns("i.file4"),
                "3. Indique o diretório base primária da CNEN",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xlsx",
                           ".xls",
                           ".ods")
              ),


              downloadButton(ns("i.download4"), "Executar Tratamento e Baixar Dataset"),
              width = 10
            ),
            #fim conditional panel

            conditionalPanel(
              condition = "input.id2 == 'Fonte FINEP'",
              fileInput(
                ns("i.file5"),
                "3. Indique o diretório da base primária da FINEP",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".ods",
                           ".xlsx",
                           ".xls")
              ),

              downloadButton(ns("i.download5"), "Executar Tratamento e Baixar Dataset"),
              width = 10
            ),#fim conditional panel
            conditionalPanel(
              condition = "input.id2 == 'Fonte FAPESP'",
              fileInput(
                ns("i.file6"),
                "3. Indique o diretório da base primária da FAPESP",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xlsx",
                           ".xls",
                           ".ods")
              ),

              downloadButton(ns("i.download6"), "Executar Tratamento e Baixar Dataset"),
              width = 10
            ),

            conditionalPanel(
              condition = "input.id2 == 'Fonte CNPQ'",
              fileInput(
                ns("i.file7"),
                "3. Indique o diretório da base primária do CNPq",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xls",
                           ".xlsx",
                           ".ods")
              ),

              downloadButton(ns("i.download7"), "Executar Tratamento e Baixar Dataset"),
              width = 10
            )

          )#Fim Fluidrow


        ),#fim do conditional panel

        width = 10
    )#fim da colum
    )#fim da fluidrow

  )#fim taglist
}

#' tratamento Server Functions
#'
#' @noRd
mod_tratamento_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #Importando dados
    options(scipen=999)
    options(shiny.maxRequestSize=120*1024^3) # Maximum upload size allowed, in bytes


    #filesqlite<- input$file_sqlite
    #if (is.null(filesqlite)) return(NULL)

#Carga Completa
    #Aneel

    myData <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_aneel(origem_processos = inFile$datapath)

      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_completo(data, fonte)

      return(data)
    })

    #Fazer o Download
    output$download <- downloadHandler(
      filename = function() {
        paste("aneel_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData(), file, row.names = FALSE)
      }
    )

    #ANP

    server <- function(input, output, session) {
      shinyBS::addTooltip(id="anp.btn",title="Hello! This is a hover pop-up. You'll have to click to see the next one.")
    }
    myData2 <- reactive({
      inFile2 <- input$file2
      if (is.null(inFile2)) return(NULL)

      data <- ETLEBP::cria_base_intermediaria_anp(origem_processos = inFile2$datapath)
      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_completo(data, fonte)

      return(data)
    })

    #Fazer o Download
    output$download2 <- downloadHandler(
      filename = function() {
        paste("anp_interm", ".csv", sep = "")
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

      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_completo(data, fonte)

      return(data)
    })

    #Fazer o Download
    output$download3 <- downloadHandler(
      filename = function() {
        paste("bndes_interm", ".csv", sep = "")
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

      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_completo(data, fonte)

      return(data)
    })

    #Fazer o Download
    output$download4 <- downloadHandler(
      filename = function() {
        paste("cnen_interm", ".csv", sep = "")
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

      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_completo(data, fonte)

      return(data)
    })

    #Fazer o Download
    output$download5 <- downloadHandler(
      filename = function() {
        paste("finep_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData5(), file, row.names = FALSE)
      }
    )

    #FAPESP

    myData6 <- reactive({
      inFile6 <- input$file6
      if (is.null(inFile6)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_fapesp(origem_processos = inFile6$datapath)

      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_completo(data, fonte)

      return(data)
    })

    #Fazer o Download
    output$download6 <- downloadHandler(
      filename = function() {
        paste("fapesp_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData6(), file, row.names = FALSE)
      }
    )

   #CNPQ

    myData7<- reactive({
      inFile7 <- input$file7
      if (is.null(inFile7)) return(NULL)

      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_cnpq(origem_processos = inFile7$datapath)

      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_completo(data, fonte)

      return(data)
    })

    #Fazer o Download
    output$download7 <- downloadHandler(
      filename = function() {
        paste("cnpq_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData7(), file, row.names = FALSE)
      }
    )
    #Carga incremental

    #ANEEL

    i.myData <- reactive({
      i.inFile1 <- input$i.file1
      if (is.null(i.inFile1)) return(NULL)

      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_aneel(origem_processos = i.inFile1$datapath)
      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_incremental(data, filename)

      return(data)
    })

    #Fazer o Download
    output$i.download <- downloadHandler(
      filename = function() {
        paste("aneel_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(i.myData(), file, row.names = FALSE)
      }
    )

#ANP

    i.myData2 <- reactive({
      i.inFile2 <- input$i.file2
      if (is.null(i.inFile2)) return(NULL)
      #i.inFile22 <- input$i.file22
      #if (is.null(i.inFile22)) return(NULL)

      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))

      data <- ETLEBP::cria_base_intermediaria_anp(origem_processos = i.inFile2$datapath)
      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_incremental(data, filename)

      return(data)
    })

    #Fazer o Download
    output$i.download2 <- downloadHandler(
      filename = function() {
        paste("anp_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(i.myData2(), file, row.names = FALSE)
      }
    )
    #BNDES
    i.myData3 <- reactive({
      i.inFile3 <- input$i.file3
      if (is.null(i.inFile3)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_bndes(origem_processos = i.inFile3$datapath)

      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_incremental(data, filename)

      return(data)
    })

    #Fazer o Download
    output$i.download3 <- downloadHandler(
      filename = function() {
        paste("bndes_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(i.myData3(), file, row.names = FALSE)
      }
    )
    #CNEN
    i.myData4 <- reactive({
      i.inFile4 <- input$i.file4
      if (is.null(i.inFile4)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_cnen(origem_processos = i.inFile4$datapath)

      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_incremental(data, filename)

      return(data)
    })

    #Fazer o Download
    output$i.download4 <- downloadHandler(
      filename = function() {
        paste("cnen_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(i.myData4(), file, row.names = FALSE)
      }
    )
    #FINEP
    i.myData5 <- reactive({
      i.inFile5 <- input$i.file5
      if (is.null(i.inFile5)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_finep(origem_processos = i.inFile5$datapath)

      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_incremental(data, filename)

      return(data)
    })

    #Fazer o Download
    output$i.download5 <- downloadHandler(
      filename = function() {
        paste("finep_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(i.myData5(), file, row.names = FALSE)
      }
    )

    #FAPESP

    i.myData6 <- reactive({
      i.inFile6 <- input$i.file6
      if (is.null(i.inFile6)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_fapesp(origem_processos = i.inFile6$datapath)

      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_incremental(data, filename)

      return(data)
    })

    #Fazer o Download
    output$i.download6 <- downloadHandler(
      filename = function() {
        paste("fapesp_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(i.myData6(), file, row.names = FALSE)
      }
    )


    #CNPQ

    i.myData7 <- reactive({
      i.inFile7 <- input$i.file7
      if (is.null(i.inFile7)) return(NULL)

      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_cnpq(origem_processos = i.inFile7$datapath)

      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_incremental(data, filename)
      #
      # con <- DBI::dbConnect(RSQLite::SQLite(),
      #                       ":memory:",
      #                       dbname = filename)
      # #importando tabela com os titulos de projeto
      # mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
      #
      # data <- data %>%
      #   dplyr::mutate(existe = ifelse(titulo_projeto %in% mytbl6$'título',
      #                                 "sim",
      #                                 "não"))

      return(data)
    })

    #Fazer o Download
    output$i.download7 <- downloadHandler(
      filename = function() {
        paste("cnpq_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(i.myData7(), file, row.names = FALSE)
      }
    )

  })
}

## To be copied in the UI
# mod_tratamento_ui("tratamento_1")

## To be copied in the server
# mod_tratamento_server("tratamento_1")
