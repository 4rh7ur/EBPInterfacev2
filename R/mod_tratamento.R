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
        "Selecione o Tipo de Carga",
        choices = c("Carga Completa", "Carga Incremental")
      ),
      width = 10
    )),

    #Iniciando Carga Completa
    fluidRow(column(
      conditionalPanel(
        condition = "input.id0 == 'Carga Completa'",
        selectInput(
          "id1",
          "1. Selecione a Fonte dos Dados que Receberá o Tratamento",
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
            condition = "input.id1 == 'Fonte ANEEL'",
            tags$div(style = "display:inline-block",
                     title ="É importante selecionar duas bases para a atualização completa dos dados.
                     A base PD Busca Textual contém todos projetos de P&D com as informações de dispêndio e descrição dos projetos e a base RELATORIO PD RF ENTIDADES adiciona informação sobre os agentes executores dos projetos.",
                     actionButton("btn", "",
                                  icon = icon("question", lib = "font-awesome"))
            ),
            fileInput(
              ns("file1"),
              "Indique o diretório de PD Busca Textual",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
            ),
            fileInput(
              ns("file11"),
              "Indique o diretório de 5.PD RF EQUIPE",
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

            tags$div(style = "display:inline-block",
                     title ="
                     É importante selecionar duas bases para a atualização completa dos dados.
                     A base de dados agregados contém a informação dos dispêndios declarados pelas empresas até 2018 e a base rt3 possui informações dos projetos a partir do ano de 2017.",
                     actionButton("anp.btn", "",
                     icon = icon("question", lib = "font-awesome"))
                     ),

            fileInput(
              ns("file2"),
              "Indique o diretório de projetos-rt-3-2015",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
            ),
            fileInput(
              ns("file22"),
              "Indique o diretório de anp_agregados_declarados",
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
              "Indique o diretório de naoautomaticas",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
            ),


            downloadButton(ns("download3"), "Executar Tratamento e Baixar Dataset"),
            width = 10
          ),

          conditionalPanel(
            condition = "input.id1 == 'Fonte CNEN'",
            fileInput(
              ns("file4"),
              "Indique o diretório de Projeto CNEN_Plataforma Inova-E",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
            ),


            downloadButton(ns("download4"), "Executar Tratamento e Baixar Dataset"),
            width = 10
          ),

          conditionalPanel(
            condition = "input.id1 == 'Fonte FINEP'",
            fileInput(
              ns("file5"),
              "Indique o diretório de 14_09_2021_Liberacoes",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
            ),

            downloadButton(ns("download5"), "Executar Tratamento e Baixar Dataset"),
            width = 10
          ),
          conditionalPanel(
            condition = "input.id1 == 'Fonte FAPESP'",
            fileInput(
              ns("file6"),
              "Indique o diretório de PROJETOS FAPESP SELECIONADOS INOVA-E - VALORES - 13 dez 2021",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
            ),

            downloadButton(ns("download6"), "Executar Tratamento e Baixar Dataset"),
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
              tags$div(style = "display:inline-block",
                       title ="É importante selecionar duas bases para a atualizaçãocompleta dos dados.
                       A base PD Busca Textual contém todos projetos de P&D com as informações de dispêndio e descrição dos projetos e a base RELATORIO PD RF ENTIDADES adiciona informação sobre os agentes executores dos projetos.",
                       actionButton("i.btn", "",
                                    icon = icon("question", lib = "font-awesome"))
              ),
              fileInput(
                ns("i.file1"),
                "2.Indique o diretório de PD Busca Textual",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              ),
              fileInput(
                ns("i.file11"),
                "Indique o diretório de 5.PD RF EQUIPE",
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
              tags$div(style = "display:inline-block",
                       title ="
                     É importante selecionar duas bases para a atualização completa dos dados.
                     A base de dados agregados contém a informação dos dispêndios declarados pelas empresas até 2018 e a base rt3 possui informações dos projetos a partir do ano de 2017.",
                       actionButton("i.anp.btn", "",
                                    icon = icon("question", lib = "font-awesome"))
              ),
              fileInput(
                ns("i.file2"),
                "Indique o diretório de projetos-rt-3-2015",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              ),
              fileInput(
                ns("i.file22"),
                "Indique o diretório de anp_agregados_declarados",
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
                "Indique o diretório de naoautomaticas",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              ),


              downloadButton(ns("i.download3"), "Executar Tratamento e Baixar Dataset"),
              width = 10
            ),
            #fim conditional panel

            conditionalPanel(
              condition = "input.id2 == 'Fonte CNEN'",
              fileInput(
                ns("i.file4"),
                "Indique o diretório de Projeto CNEN_Plataforma Inova-E",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              ),


              downloadButton(ns("i.download4"), "Executar Tratamento e Baixar Dataset"),
              width = 10
            ),
            #fim conditional panel

            conditionalPanel(
              condition = "input.id2 == 'Fonte FINEP'",
              fileInput(
                ns("i.file5"),
                "Indique o diretório de 14_09_2021_Liberacoes",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              ),

              downloadButton(ns("i.download5"), "Executar Tratamento e Baixar Dataset"),
              width = 10
            ),#fim conditional panel
            conditionalPanel(
              condition = "input.id2 == 'Fonte FAPESP'",
              fileInput(
                ns("i.file6"),
                "Indique o diretório de PROJETOS FAPESP SELECIONADOS INOVA-E - VALORES - 13 dez 2021",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              ),

              downloadButton(ns("i.download6"), "Executar Tratamento e Baixar Dataset"),
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
    options(shiny.maxRequestSize=80*1024^3) # Maximum upload size allowed, in bytes

#Carga Completa
    #Aneel


    myData <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      inFile11 <- input$file11
      if (is.null(inFile11)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_aneel(origem_processos = inFile$datapath,
                                                    origem_equipes = inFile11$datapath)

      fonte <- "data/DB_EIP/EIP_20210415.db"

      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = fonte)
      mytbl2 <- DBI::dbReadTable(con,"dm_categoria")
      mytbl3 <- DBI::dbReadTable(con,"dm_formentador")
      mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
      mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

      #inserir categorias IEA do sqlite na base

      #criando um objeto com informações sobre as categorias de IEA
      consulta <- dplyr::select(mytbl7, id_item, id_cat2, id_formnt)

      #puxando as descrições das categorias
      consulta <- dplyr::left_join(consulta, mytbl2[,c("id","cat2")],
                            by = c("id_cat2" = "id"))
      #trazendo as informações de fomentador
      consulta <- dplyr::left_join(consulta, mytbl3[,c("id_formentador","nme_form")],
                            by = c("id_formnt"= "id_formentador"))

      #trazendo as informações de titulo
      consulta <- dplyr::left_join(consulta, mytbl6[,c("título", "id_item")])

      #fazendo o merge
      data <- dplyr::left_join(data, consulta[,c ("título", "cat2") ],
                        by = c("titulo_projeto"= "título")) %>% unique()
      # criar uma coluna que diz se o caso já existe no sqlite
      data <- data %>%
        dplyr::mutate(existe = ifelse(titulo_projeto %in% mytbl6$título,
                               "sim",
                               "não"))


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

    server <- function(input, output, session) {
      addTooltip(id="anp.btn",title="Hello! This is a hover pop-up. You'll have to click to see the next one.")
    }
    myData2 <- reactive({
      inFile2 <- input$file2
      if (is.null(inFile2)) return(NULL)
      inFile22 <- input$file22
      if (is.null(inFile22)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_anp(origem_processos = inFile2$datapath,
                                                  origem_enriquecimento = inFile22$datapath)
      fonte <- "data/DB_EIP/EIP_20210415.db"

      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = fonte)
      mytbl2 <- DBI::dbReadTable(con,"dm_categoria")
      mytbl3 <- DBI::dbReadTable(con,"dm_formentador")
      mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

      consulta <- dplyr::select(mytbl7, id_item, id_cat2, id_formnt)
      m2<-mytbl2 %>% dplyr::select(id,cat2)
      m3 <- mytbl3 %>% dplyr::select(id_formentador,nme_form)
      consulta <- dplyr::left_join(consulta, m2,by = c("id_cat2" = "id"))
      consulta <- dplyr::left_join(consulta, m3, by = c("id_formnt"= "id_formentador"))

      consulta <- consulta %>% dplyr::mutate(id_projeto = paste(nme_form, id_item , sep = "-"))

      consulta<-consulta %>% dplyr::select(id_projeto,cat2)

      data <- dplyr::left_join(data, consulta, by = c("id"= "id_projeto"))
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

      fonte <- "data/DB_EIP/EIP_20210415.db"

      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = fonte)
      mytbl2 <- DBI::dbReadTable(con,"dm_categoria")
      mytbl3 <- DBI::dbReadTable(con,"dm_formentador")
      mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

      consulta <- dplyr::select(mytbl7, id_item, id_cat2, id_formnt)
      m2<-mytbl2 %>% dplyr::select(id,cat2)
      m3 <- mytbl3 %>% dplyr::select(id_formentador,nme_form)
      consulta <- dplyr::left_join(consulta, m2,by = c("id_cat2" = "id"))
      consulta <- dplyr::left_join(consulta, m3, by = c("id_formnt"= "id_formentador"))

      consulta <- consulta %>% dplyr::mutate(id_projeto = paste(nme_form, id_item , sep = "-"))

      consulta<-consulta %>% dplyr::select(id_projeto,cat2)

      data <- dplyr::left_join(data, consulta, by = c("id"= "id_projeto"))

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

      fonte <- "data/DB_EIP/EIP_20210415.db"

      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = fonte)
      mytbl2 <- DBI::dbReadTable(con,"dm_categoria")
      mytbl3 <- DBI::dbReadTable(con,"dm_formentador")
      mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

      consulta <- dplyr::select(mytbl7, id_item, id_cat2, id_formnt)
      m2<-mytbl2 %>% dplyr::select(id,cat2)
      m3 <- mytbl3 %>% dplyr::select(id_formentador,nme_form)
      consulta <- dplyr::left_join(consulta, m2,by = c("id_cat2" = "id"))
      consulta <- dplyr::left_join(consulta, m3, by = c("id_formnt"= "id_formentador"))

      consulta <- consulta %>% dplyr::mutate(id_projeto = paste(nme_form, id_item , sep = "-"))

      consulta<-consulta %>% dplyr::select(id_projeto,cat2)

      data <- dplyr::left_join(data, consulta, by = c("id"= "id_projeto"))

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

      fonte <- "data/DB_EIP/EIP_20210415.db"

      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = fonte)
      mytbl2 <- DBI::dbReadTable(con,"dm_categoria")
      mytbl3 <- DBI::dbReadTable(con,"dm_formentador")
      mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

      consulta <- dplyr::select(mytbl7, id_item, id_cat2, id_formnt)
      m2<-mytbl2 %>% dplyr::select(id,cat2)
      m3 <- mytbl3 %>% dplyr::select(id_formentador,nme_form)
      consulta <- dplyr::left_join(consulta, m2,by = c("id_cat2" = "id"))
      consulta <- dplyr::left_join(consulta, m3, by = c("id_formnt"= "id_formentador"))

      consulta <- consulta %>% dplyr::mutate(id_projeto = paste(nme_form, id_item , sep = "-"))

      consulta<-consulta %>% dplyr::select(id_projeto,cat2)

      data <- dplyr::left_join(data, consulta, by = c("id"= "id_projeto"))

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

    #FAPESP

    myData6 <- reactive({
      inFile6 <- input$file6
      if (is.null(inFile6)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_finep(origem_processos = inFile6$datapath)

      fonte <- "data/DB_EIP/EIP_20210415.db"

      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = fonte)
      mytbl2 <- DBI::dbReadTable(con,"dm_categoria")
      mytbl3 <- DBI::dbReadTable(con,"dm_formentador")
      mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

      consulta <- dplyr::select(mytbl7, id_item, id_cat2, id_formnt)
      m2<-mytbl2 %>% dplyr::select(id,cat2)
      m3 <- mytbl3 %>% dplyr::select(id_formentador,nme_form)
      consulta <- dplyr::left_join(consulta, m2,by = c("id_cat2" = "id"))
      consulta <- dplyr::left_join(consulta, m3, by = c("id_formnt"= "id_formentador"))

      consulta <- consulta %>% dplyr::mutate(id_projeto = paste(nme_form, id_item , sep = "-"))

      consulta<-consulta %>% dplyr::select(id_projeto,cat2)

      data <- dplyr::left_join(data, consulta, by = c("id"= "id_projeto"))

      return(data)
    })

    #Fazer o Download
    output$download6 <- downloadHandler(
      filename = function() {
        paste(myData6(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData6(), file, row.names = FALSE)
      }
    )
    #Carga incremental

    #ANEEL

    i.myData <- reactive({
      i.inFile1 <- input$i.file1
      if (is.null(i.inFile1)) return(NULL)
      i.inFile11 <- input$i.file11
      if (is.null(i.inFile11)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_aneel(origem_processos = i.inFile1$datapath,
                                                    origem_equipes = i.inFile11$datapath)
      #Criando dataset com casos novos
      filename <- "data/DB_EIP/EIP_20210415.db"
      con <- DBI::dbConnect(RSQLite::SQLite(),
                       ":memory:",
                       dbname = filename)
      #importando tabela com os titulos de projeto
      mytbl6 <- ETLEBP::dbReadTable(con,"dm_projeto")



      return(data)
    })

    #Fazer o Download
    output$i.download <- downloadHandler(
      filename = function() {
        paste(i.myData(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(i.myData(), file, row.names = FALSE)
      }
    )

#ANP

    i.myData2 <- reactive({
      i.inFile2 <- input$i.file2
      if (is.null(i.inFile2)) return(NULL)
      i.inFile22 <- input$i.file22
      if (is.null(i.inFile22)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_anp(origem_processos = i.inFile2$datapath,
                                                  origem_enriquecimento = i.inFile22$datapath)
      #Criando dataset com casos novos
      filename <- "data/DB_EIP/EIP_20210415.db"
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)
      #importando tabela com os titulos de projeto
      mytbl6 <- ETLEBP::dbReadTable(con,"dm_projeto")



      return(data)
    })

    #Fazer o Download
    output$i.download2 <- downloadHandler(
      filename = function() {
        paste(i.myData2(), ".csv", sep = "")
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
      filename <- "data/DB_EIP/EIP_20210415.db"
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)
      #importando tabela com os titulos de projeto
      mytbl6 <- ETLEBP::dbReadTable(con,"dm_projeto")



      return(data)
    })

    #Fazer o Download
    output$i.download3 <- downloadHandler(
      filename = function() {
        paste(i.myData3(), ".csv", sep = "")
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
      filename <- "data/DB_EIP/EIP_20210415.db"
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)
      #importando tabela com os titulos de projeto
      mytbl6 <- ETLEBP::dbReadTable(con,"dm_projeto")



      return(data)
    })

    #Fazer o Download
    output$i.download4 <- downloadHandler(
      filename = function() {
        paste(i.myData4(), ".csv", sep = "")
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
      filename <- "data/DB_EIP/EIP_20210415.db"
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)
      #importando tabela com os titulos de projeto
      mytbl6 <- ETLEBP::dbReadTable(con,"dm_projeto")



      return(data)
    })

    #Fazer o Download
    output$i.download5 <- downloadHandler(
      filename = function() {
        paste(i.myData5(), ".csv", sep = "")
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
      data <- ETLEBP::cria_base_intermediaria_finep(origem_processos = i.inFile6$datapath)

      #Criando dataset com casos novos
      filename <- "data/DB_EIP/EIP_20210415.db"
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)
      #importando tabela com os titulos de projeto
      mytbl6 <- ETLEBP::dbReadTable(con,"dm_projeto")



      return(data)
    })

    #Fazer o Download
    output$i.download6 <- downloadHandler(
      filename = function() {
        paste(i.myData6(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(i.myData6(), file, row.names = FALSE)
      }
    )
  })
}

## To be copied in the UI
# mod_tratamento_ui("tratamento_1")

## To be copied in the server
# mod_tratamento_server("tratamento_1")
