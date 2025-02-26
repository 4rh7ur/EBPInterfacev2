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
    # Identificar o Tipo de Carga ----
    fluidRow(column(
      selectInput(
        "id0",
        "1. Selecione o Tipo de Carga",
        choices = c("Carga Completa", "Carga Incremental")
      ),
      fileInput(ns("file_sqlite"), "Indique o diretório da base SQLite"),
      width = 10
      )),

    # fluidRow(column(width = 12,
    #        uiOutput("sheet_selector"),
    #        # Seleção das colunas para cada variável esperada
    #        uiOutput("var_selectors"),
    #        # Mostrar dados com base nas seleções
    #        tableOutput("conteudo"))),  # Seletor de sheet dinâmico),

    #Iniciando Carga Completa ----
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
        fluidRow(column(width = 10,
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
                          uiOutput(ns("sheet_selector1c")),

                          uiOutput(ns('var_selectors1c')),

                          tableOutput(ns('conteudo1c')),

                          downloadButton(ns("download"), "Executar Tratamento e Baixar Dataset"),
                          width = 10
                        ),

                        conditionalPanel(
                          condition = "input.id1 == 'Fonte ANP'",
                          fileInput(
                            ns("file2"),
                            " 3. Indique o diretório da base primária da ANP",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")
                          ),

                          uiOutput(ns("sheet_selector2c")),

                          uiOutput(ns('var_selectors2c')),

                          tableOutput(ns('conteudo2c')),

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

                          uiOutput(ns("sheet_selector3c")),

                          uiOutput(ns('var_selectors3c')),

                          tableOutput(ns('conteudo3c')),


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
                                       ".ods"),
                            ),
                          uiOutput(ns("sheet_selector4c")),

                          uiOutput(ns('var_selectors4c')),

                          tableOutput(ns('conteudo4c')),


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
                          uiOutput(ns("sheet_selector5c")),

                          uiOutput(ns('var_selectors5c')),

                          tableOutput(ns('conteudo5c')),

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

                          uiOutput(ns("sheet_selector6c")),

                          uiOutput(ns('var_selectors6c')),

                          tableOutput(ns('conteudo6c')),

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

                          uiOutput(ns("sheet_selector7c")),

                          uiOutput(ns('var_selectors7c')),

                          tableOutput(ns('conteudo7c')),

                          downloadButton(ns("download7"), "Executar Tratamento e Baixar Dataset"),
                          width = 10
                        )
        )
        )
      ),#fim conditional panel

      width = 10
    )#fim column
    ), #fim fluidrow

    #Iniciando Carga Incremental ----
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

          fluidRow(column(width = 10,
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

                            uiOutput(ns("sheet_selector1")),

                            uiOutput(ns('var_selectors1')),

                            tableOutput(ns('conteudo1')),

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

                            uiOutput(ns("sheet_selector2")),

                            uiOutput(ns('var_selectors2')),

                            tableOutput(ns('conteudo2')),

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
                            uiOutput(ns("sheet_selector3")),

                            uiOutput(ns('var_selectors3')),

                            tableOutput(ns('conteudo3')),


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
                            uiOutput(ns("sheet_selector4")),

                            uiOutput(ns('var_selectors4')),

                            tableOutput(ns('conteudo4')),


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
                            uiOutput(ns("sheet_selector5")),

                            uiOutput(ns('var_selectors5')),

                            tableOutput(ns('conteudo5')),


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
                            uiOutput(ns("sheet_selector6")),

                            uiOutput(ns('var_selectors6')),

                            # tableOutput(ns('conteudo6')),

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
                            uiOutput(ns("sheet_selector7")),

                            uiOutput(ns('var_selectors7')),

                            # tableOutput(ns('conteudo7')),

                            downloadButton(ns("i.download7"), "Executar Tratamento e Baixar Dataset"),
                            width = 10
                          )
          )

          )#Fim Fluidrow


        ),#fim do conditional panel

        width = 10
      )#fim da colum
    )#fim da fluidrow ,


  )#fim taglist
}

#' tratamento Server Functions
#'
#' @noRd
mod_tratamento_server <- function(id){

  if (!requireNamespace(c("readxl","rio", "tools","readr","readODS", "tidyr","dplyr", "abjutils"), quietly = TRUE)) {
  install.packages(c("readxl","rio", "tools","readr","readODS", "tidyr","dplyr","abjutils"), dependencies = TRUE)
}

  library(readxl)
  library(rio)
  library(tools)
  library(readr)
  library(readODS)
  library(tidyr)
  library(dplyr)
  library(abjutils)




  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #Importando dados
    options(scipen=999)
    options(shiny.maxRequestSize=120*1024^3) # Maximum upload size allowed, in bytes


    # filesqlite<- input$file_sqlite
    # if (is.null(filesqlite)) return(NULL)


    # # Carregar o arquivo de variáveis esperadas e preparar a estrutura para mapeamento
    # library(here)
    # local= here::here('EBP/ETLEBP2023/data/var_utils_ebp_etl.xlsx')
    # var <- readxl::read_xlsx(local)
    var <- data.frame(
      `Fonte ANP` = c("Valor da Cláusula", "Data de início", "Prazo em meses", "Título", "Objetivo", "Tema", "Subtema", "Número ANP", "Empresa responsável", "Executor", "Qualificação", "Área", NA, NA),
      `Fonte ANEEL` = c("Ano de cadastro da proposta do projeto", "Data de conclusão do projeto", "Quantidade de meses de duração prevista", "Valor de custo total previsto", "Valor de custo total auditado", "Descrição do título do projeto", "Sigla do segmento do setor elétrico", "Sigla do tema do projeto", "Nome do agente proponente", "Situação do projeto", "Código do projeto ANEEL", "Sigla FAS Inovação do Projeto", NA, NA),
      `Fonte BNDES` = c("Inovação", "CNPJ", "Unidade da Federação", "Número do contrato", "Data da contratação", "Prazo de amortização em meses", "Prazo de carência em meses", "Valor contratado", "Produto", "Modalidade de apoio", "Descrição do projeto", "Situação do contrato", "Cliente", "Natureza do cliente"),
      `Fonte CNEN` = c("Data de assinatura", "Data limite", "Categoria da tecnologia (dígito 2)", "Valor contratado", "Título do projeto", "Nome do agente executor", "Natureza do agente financiador", "Natureza do financiamento", "Natureza do agente executor", "Modalidade do financiamento", "UF de execução", "P&D ou Demonstração", "ID", NA),
      `Fonte CNPQ` = c("Grande área do conhecimento", "Área do conhecimento", "Subárea do conhecimento", "Ano de referência", "Título do projeto", "Categoria do nível", "Sigla da UF do destino", "Região do destino", "País do destino", "Valor pago", "Sigla da UF da origem", "Processo", "Instituição Destino", NA),
      `Fonte FAPESP` = c("Número do processo", "Data de início", "Data de término", "Título em português", "Área do conhecimento", "Subárea do conhecimento", "Valor concedido", "Beneficiário", NA, NA, NA, NA, NA,NA),
      `Fonte FINEP` = c("Título", "Valor FINEP", "Valor liberado", "Data da assinatura", "Prazo de utilização", "Contrato", "Instrumento", "Proponente", "UF do Executor", "Status do projeto", NA, NA, NA,NA),
      stringsAsFactors = FALSE
    )

    names(var) <- c("Fonte ANP", "Fonte ANEEL", "Fonte BNDES", "Fonte CNEN", "Fonte CNPQ",
                    "Fonte FAPESP", "Fonte FINEP" )
    vars <- gather(var, key = "Base", value = "Variável", na.rm = TRUE) %>% as.data.frame()
    #

    #Carga Completa ----
    #Aneel ----

    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets1c <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$file1$datapath)

      ext <- tools::file_ext(input$file1$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$file1$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$file1$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector1c <- renderUI({
      req(sheets1c())
      selectInput(ns("sheet1c"), "Escolha a aba (sheet):", choices = sheets1c()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados1c <- reactive({
      req(input$file1$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$file1$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$file1$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$file1$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$file1$datapath, sheet = input$sheet1c, n_max = 15, col_names = F)
      } else if (ext == "ods") {
        preview <- read_ods(input$file1$datapath, sheet = input$sheet1c, n_max = 15, col_names = F)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$file1$datapath, fread_args = list(skip = first_valid_row-1), encoding = 'Latin-1')
        }, error = function(e) {
          readr::read_csv2(input$file1$datapath, skip = first_valid_row-1)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$file1$datapath, sheet = input$sheet1c, skip = first_valid_row-1)
      } else if (ext == "ods") {
        dados <- read_ods(input$file1$datapath, sheet = input$sheet1c, skip = first_valid_row-1)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors1c <- renderUI({

      req(dados1c())  # Garante que os dados foram carregados
      df1c <- dados1c()

      # Nomes das colunas do dataframe
      col_names <- names(df1c)

      # Definir as variáveis esperadas
      variaveis_esperadas1c <- subset(vars, vars$Base == 'Fonte ANEEL')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas1c, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_c_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo1c <- reactive({
      req(dados1c())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas1c <- subset(vars, vars$Base == "Fonte ANEEL")[,2]

      colunas_selecionadas1c <- sapply(variaveis_esperadas1c, function(var) {
        input[[paste0("select_c_", var)]]
      })

      # Renomear as colunas do dataframe conforme o mapeamento
      df1c <- dados1c()
      df1c <- df1c %>% rename(ano_cadastro_proposta_projeto = colunas_selecionadas1c[[1]],
                            dat_conclusao_projeto = colunas_selecionadas1c[[2]],
                            qtd_meses_duracao_prevista = colunas_selecionadas1c[[3]],
                            vlr_custo_total_previsto = colunas_selecionadas1c[[4]],
                            vlr_custo_total_auditado = colunas_selecionadas1c[[5]],
                            dsc_titulo_projeto = colunas_selecionadas1c[[6]],
                            sig_segmento_setor_eletrico = colunas_selecionadas1c[[7]],
                            sig_tema_projeto = colunas_selecionadas1c[[8]],
                            nom_agente = colunas_selecionadas1c[[9]],
                            idc_situacao_projeto = colunas_selecionadas1c[[10]],
                            cod_proj = colunas_selecionadas1c[[11]],
                            sig_fas_inovacao_projeto = colunas_selecionadas1c[[12]])
      # colnames(df) <- colunas_selecionadas4

    })

    #############################################

    myData <- reactive({
      req(conteudo1c())
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_aneel(origem_processos = conteudo1c())
      #data <- cria_base_intermediaria_aneel(origem_processos = conteudo1c())

      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_completo(data, fonte)
      #data <- executa_tratamento_completo(data, fonte)


      return(data)
    })

    #Fazer o Download
    output$download <- downloadHandler(
      filename = function() {
        paste("aneel_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv2(myData(), file, row.names = FALSE, sep = ";", fileEncoding = "Latin1")
      }
    )

    #ANP ----

    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets2c <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$file2$datapath)

      ext <- tools::file_ext(input$file2$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$file2$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$file2$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector2c <- renderUI({
      req(sheets2c())
      selectInput(ns("sheet2c"), "Escolha a aba (sheet):", choices = sheets2c()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados2c <- reactive({
      req(input$file2$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$file2$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$file2$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$file2$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$file2$datapath, sheet = input$sheet2c, n_max = 15, col_names = F)
      } else if (ext == "ods") {
        preview <- read_ods(input$file2$datapath, sheet = input$sheet2c, n_max = 15, col_names = F)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$file2$datapath, fread_args = list(skip = first_valid_row-1))
        }, error = function(e) {
          readr::read_csv2(input$file2$datapath, skip = first_valid_row-1)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$file2$datapath, sheet = input$sheet2c, skip = first_valid_row-1)
      } else if (ext == "ods") {
        dados <- read_ods(input$file2$datapath, sheet = input$sheet2c, skip = first_valid_row-1)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors2c <- renderUI({

      req(dados2c())  # Garante que os dados foram carregados
      df2c <- dados2c()

      # Nomes das colunas do dataframe
      col_names <- names(df2c)

      # Definir as variáveis esperadas
      variaveis_esperadas2c <- subset(vars, vars$Base == 'Fonte ANP')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas2c, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_c_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo2c <- reactive({
      req(dados2c())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas2c <- subset(vars, vars$Base == "Fonte ANP")[,2]

      colunas_selecionadas2c <- sapply(variaveis_esperadas2c, function(var) {
        input[[paste0("select_c_", var)]]
      })

      # Renomear as colunas do dataframe conforme o mapeamento
      df2c <- dados2c()
      df2c <- df2c %>% rename(valor_clausula = colunas_selecionadas2c[[1]],
                            data_inicio = colunas_selecionadas2c[[2]],
                            prazo = colunas_selecionadas2c[[3]],
                            titulo = colunas_selecionadas2c[[4]],
                            objetivo = colunas_selecionadas2c[[5]],
                            tema = colunas_selecionadas2c[[6]],
                            subtema = colunas_selecionadas2c[[7]],
                            no_anp = colunas_selecionadas2c[[8]],
                            empresa_responsavel = colunas_selecionadas2c[[9]],
                            executor = colunas_selecionadas2c[[10]],
                            qualificacao = colunas_selecionadas2c[[11]],
                            area = colunas_selecionadas2c[[12]])
      # colnames(df) <- colunas_selecionadas4

    })

    #############################################

    # server <- function(input, output, session) {
    #   shinyBS::addTooltip(id="anp.btn",title="Hello! This is a hover pop-up. You'll have to click to see the next one.")
    #
    # }
    #
    # dados <- reactive({
    #   req(input$file2)  # Garante que um arquivo foi carregado
    #   read.csv(input$arquivo$datapath)  # Ler o arquivo CSV
    # })
    #
    # # Gerar os inputs de seleção dinamicamente
    # output$var_selectors <- renderUI({
    #   req(dados())  # Garante que os dados foram carregados
    #   df <- dados()
    #
    #   # Nomes das colunas do dataframe
    #   col_names <- names(df)
    #
    #   # Definir as variáveis esperadas (exemplo)
    #   variaveis_esperadas <- c("Nome", "Idade", "Salario")
    #
    #   # Criar um selectInput para cada variável esperada
    #   inputs <- lapply(variaveis_esperadas, function(var) {
    #     selectInput(
    #       inputId = paste0("select_", var),
    #       label = paste("Selecione a coluna para", var, ":"),
    #       choices = col_names,
    #       selected = NULL
    #     )
    #   })
    #
    #   # Exibir os selectInput na interface
    #   do.call(tagList, inputs)
    # })


      myData2 <- reactive({
        req(conteudo2c())
      inFile2 <- input$file2
      if (is.null(inFile2)) return(NULL)

      data <- ETLEBP::cria_base_intermediaria_anp(origem_processos = conteudo2c())
      #data <- cria_base_intermediaria_anp(origem_processos = conteudo2c())


      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_completo(data, fonte)
      #data <- executa_tratamento_completo(data, fonte)

      return(data)
    })

    #Fazer o Download
    output$download2 <- downloadHandler(
      filename = function() {
        paste("anp_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv2(myData2(), file, row.names = FALSE, fileEncoding = 'Latin1')
      }
    )
    #BNDES----

    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets3c <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$file3$datapath)

      ext <- tools::file_ext(input$file3$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$file3$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$file3$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector3c <- renderUI({
      req(sheets3c())
      selectInput(ns("sheet3c"), "Escolha a aba (sheet):", choices = sheets3c()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados3c <- reactive({
      req(input$file3$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$file3$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$file3$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$file3$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$file3$datapath, sheet = input$sheet3c, n_max = 15, col_names = F)
      } else if (ext == "ods") {
        preview <- read_ods(input$file3$datapath, sheet = input$sheet3c, n_max = 15, col_names = F)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$file3$datapath, fread_args = list(skip = first_valid_row-1), encoding = 'Latin-1')
        }, error = function(e) {
          readr::read_csv2(input$file3$datapath, skip = first_valid_row-1)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$file3$datapath, sheet = input$sheet3c, skip = first_valid_row-1)
      } else if (ext == "ods") {
        dados <- read_ods(input$file3$datapath, sheet = input$sheet3c, skip = first_valid_row-1)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors3c <- renderUI({

      req(dados3c())  # Garante que os dados foram carregados
      df3c <- dados3c()

      # Nomes das colunas do dataframe
      col_names <- names(df3c)

      # Definir as variáveis esperadas
      variaveis_esperadas3c <- subset(vars, vars$Base == 'Fonte BNDES')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas3c, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_c_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo3c <- reactive({
      req(dados3c())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas3c <- subset(vars, vars$Base == "Fonte BNDES")[,2]

      colunas_selecionadas3c <- sapply(variaveis_esperadas3c, function(var) {
        input[[paste0("select_c_", var)]]
      })

      # Renomear as colunas do dataframe conforme o mapeamento
      df3c <- dados3c()
      df3c <- df3c %>% rename(inovacao = colunas_selecionadas3c[[1]],
                            cnpj = colunas_selecionadas3c[[2]],
                            uf = colunas_selecionadas3c[[3]],
                            numero_do_contrato = colunas_selecionadas3c[[4]],
                            data_da_contratacao = colunas_selecionadas3c[[5]],
                            prazo_amortizacao_meses = colunas_selecionadas3c[[6]],
                            prazo_carencia_meses = colunas_selecionadas3c[[7]],
                            valor_contratado_r = colunas_selecionadas3c[[8]],
                            produto = colunas_selecionadas3c[[9]],
                            modalidade_de_apoio = colunas_selecionadas3c[[10]],
                            descricao_do_projeto = colunas_selecionadas3c[[11]],
                            situacao_do_contrato = colunas_selecionadas3c[[12]],
                            cliente = colunas_selecionadas3c[[13]],
                            natureza_do_cliente = colunas_selecionadas3c[[14]])
      # colnames(df) <- colunas_selecionadas4

    })

    #############################################

    myData3 <- reactive({
      req(conteudo3c())
      inFile3 <- input$file3
      if (is.null(inFile3)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_bndes(origem_processos = conteudo3c())
      #data <- cria_base_intermediaria_bndes(origem_processos = conteudo3c())

      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      data <- ETLEBP::executa_tratamento_completo(data, fonte)
      #data <- executa_tratamento_completo(data, fonte)

      return(data)
    })

    #Fazer o Download
    output$download3 <- downloadHandler(
      filename = function() {
        paste("bndes_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv2(myData3(), file, row.names = FALSE, fileEncoding = "Latin1")
      }
    )
    #CNEN----

    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets4c <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$file4$datapath)

      ext <- tools::file_ext(input$file4$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$file4$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$file4$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector4c <- renderUI({
      req(sheets4c())
      selectInput(ns("sheet4c"), "Escolha a aba (sheet):", choices = sheets4c()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados4c <- reactive({
      req(input$file4$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$file4$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$file4$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$file4$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$file4$datapath, sheet = input$sheet4c, n_max = 15, col_names = F)
      } else if (ext == "ods") {
        preview <- read_ods(input$file4$datapath, sheet = input$sheet4c, n_max = 15, col_names = F)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$file4$datapath, fread_args = list(skip = first_valid_row-1))
        }, error = function(e) {
          readr::read_csv2(input$file4$datapath, skip = first_valid_row-1)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$file4$datapath, sheet = input$sheet4c, skip = first_valid_row-1)
      } else if (ext == "ods") {
        dados <- read_ods(input$file4$datapath, sheet = input$sheet4c, skip = first_valid_row-1)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors4c <- renderUI({

      req(dados4c())  # Garante que os dados foram carregados
      df4c <- dados4c()

      # Nomes das colunas do dataframe
      col_names <- names(df4c)

      # Definir as variáveis esperadas
      variaveis_esperadas4c <- subset(vars, vars$Base == 'Fonte CNEN')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas4c, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_c_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo4c <- reactive({
      req(dados4c())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas4c <- subset(vars, vars$Base == "Fonte CNEN")[,2]

      colunas_selecionadas4c <- sapply(variaveis_esperadas4c, function(var) {
        input[[paste0("select_c_", var)]]
      })

      # Renomear as colunas do dataframe conforme o mapeamento
      df4c <- dados4c()
      df4c <- df4c %>% rename(data_assinatura = colunas_selecionadas4c[[1]],
                            data_limite = colunas_selecionadas4c[[2]],
                            categoria_da_tecnologia_digito2 = colunas_selecionadas4c[[3]],
                            valor_contratado = colunas_selecionadas4c[[4]],
                            titulo = colunas_selecionadas4c[[5]],
                            nome_do_agente_executor = colunas_selecionadas4c[[6]],
                            natureza_do_agente_financiador = colunas_selecionadas4c[[7]],
                            natureza_do_financiamento = colunas_selecionadas4c[[8]],
                            natureza_do_agente_executor = colunas_selecionadas4c[[9]],
                            modalidade_do_financiamento = colunas_selecionadas4c[[10]],
                            uf_execucao = colunas_selecionadas4c[[11]],
                            p_d_ou_demonstracao = colunas_selecionadas4c[[12]],
                            id = colunas_selecionadas4c[[13]])
      # colnames(df) <- colunas_selecionadas4

    })

    #############################################

    myData4 <- reactive({
      req(conteudo4c())
      inFile4 <- input$file4
      if (is.null(inFile4)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_cnen(origem_processos = conteudo4c())

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
        write.csv(myData4(), file, row.names = FALSE, fileEncoding = 'Latin1')
      }
    )
    #FINEP----

    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets5c <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$file5$datapath)

      ext <- tools::file_ext(input$file5$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$file5$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$file5$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector5c <- renderUI({
      req(sheets5c())
      selectInput(ns("sheet5c"), "Escolha a aba (sheet):", choices = sheets5c()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados5c <- reactive({
      req(input$file5$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$file5$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$file5$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$file5$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$file5$datapath, sheet = input$sheet5c, n_max = 15)
      } else if (ext == "ods") {
        preview <- read_ods(input$file5$datapath, sheet = input$sheet5c, n_max = 15)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$file5$datapath, fread_args = list(skip = first_valid_row))
        }, error = function(e) {
          readr::read_csv2(input$file5$datapath, skip = first_valid_row)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$file5$datapath, sheet = input$sheet5c, skip = first_valid_row)
      } else if (ext == "ods") {
        dados <- read_ods(input$file5$datapath, sheet = input$sheet5c, skip = first_valid_row)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors5c <- renderUI({

      req(dados5c())  # Garante que os dados foram carregados
      df5c <- dados5c()

      # Nomes das colunas do dataframe
      col_names <- names(df5c)

      # Definir as variáveis esperadas
      variaveis_esperadas5c <- subset(vars, vars$Base == 'Fonte FINEP')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas5c, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_c_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo5c <- reactive({
      req(dados5c())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas5c <- subset(vars, vars$Base == "Fonte FINEP")[,2]

      colunas_selecionadas5c <- sapply(variaveis_esperadas5c, function(var) {
        input[[paste0("select_c_", var)]]
      })

      # Renomear as colunas do dataframe conforme o mapeamento
      df5c <- dados5c()

      df5c <- df5c %>% rename(titulo = colunas_selecionadas5c[[1]],
                            valor_finep = colunas_selecionadas5c[[2]],
                            valor_liberado = colunas_selecionadas5c[[3]],
                            data_assinatura = colunas_selecionadas5c[[4]],
                            prazo_utilizacao = colunas_selecionadas5c[[5]],
                            contrato = colunas_selecionadas5c[[6]],
                            instrumento = colunas_selecionadas5c[[7]],
                            proponente = colunas_selecionadas5c[[8]],
                            uf = colunas_selecionadas5c[[9]],
                            status = colunas_selecionadas5c[[10]])

      # Exibir o dataframe com as colunas renomeadas
      # head(df,3)
    })
    #############################################

    myData5 <- reactive({
      req(conteudo5c())
      inFile5 <- input$file5
      if (is.null(inFile5)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      # data <- ETLEBP::cria_base_intermediaria_finep(origem_processos = inFile5$datapath)
      data <- ETLEBP::cria_base_intermediaria_finep(origem_processos = conteudo5c())

      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      # data <- ETLEBP::executa_tratamento_completo(data, fonte)
      data <- ETLEBP::executa_tratamento_completo(data, fonte)


      return(data)
    })

    #Fazer o Download
    output$download5 <- downloadHandler(
      filename = function() {
        paste("finep_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv2(myData5(), file, row.names = FALSE, fileEncoding = 'Latin1')
      }
    )

    #FAPESP-----

    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets6c <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$file6$datapath)

      ext <- tools::file_ext(input$file6$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$file6$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$file6$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector6c <- renderUI({
      req(sheets6c())
      selectInput(ns("sheet6c"), "Escolha a aba (sheet):", choices = sheets6c()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados6c <- reactive({
      req(input$file6$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$file6$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$file6$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$file6$datapath, n_max = 15,col_names = F)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$file6$datapath, sheet = input$sheet6c, n_max = 15, col_names = F)
      } else if (ext == "ods") {
        preview <- read_ods(input$file6$datapath, sheet = input$sheet6c, n_max = 15, col_names = F)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$file6$datapath, fread_args = list(skip = first_valid_row-1))
        }, error = function(e) {
          readr::read_csv2(input$file6$datapath, skip = first_valid_row-1, col_names = T)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$file6$datapath, sheet = input$sheet6c, skip = first_valid_row-1, col_names = T)
      } else if (ext == "ods") {
        dados <- read_ods(input$file6$datapath, sheet = input$sheet6c, skip = first_valid_row-1, col_names = T)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors6c <- renderUI({

      req(dados6c())  # Garante que os dados foram carregados
      df6c <- dados6c()

      # Nomes das colunas do dataframe
      col_names <- names(df6c)

      # Definir as variáveis esperadas
      variaveis_esperadas6c <- subset(vars, vars$Base == 'Fonte FAPESP')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas6c, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_c_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo6c <- reactive({
      req(dados6c())  # Garante que os dados foram carregados
      # req(input[[paste0("select_", var)]])

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas6c <- subset(vars, vars$Base == "Fonte FAPESP")[,2]

      colunas_selecionadas6c <- sapply(variaveis_esperadas6c, function(var) {
        input[[paste0("select_c_", var)]]
      })

      # Renomear as colunas do dataframe conforme o mapeamento
      df6c <- dados6c()

      df6c <- df6c %>% rename(n_processo = colunas_selecionadas6c[[1]],
                            data_de_inicio = colunas_selecionadas6c[[2]],
                            data_de_termino = colunas_selecionadas6c[[3]],
                            titulo_portugues = colunas_selecionadas6c[[4]],
                            area_do_conhecimento = colunas_selecionadas6c[[5]],
                            subarea_do_conhecimento = colunas_selecionadas6c[[6]],
                            valor_concedido = colunas_selecionadas6c[[7]],
                            beneficiario = colunas_selecionadas6c[[8]])
      # colnames(df) <- colunas_selecionadas
      # print(colunas_selecionadas)

      # Exibir o dataframe com as colunas renomeadas
      # head(df6,3)

    })
    #############################################

    myData6 <- reactive({
      req(conteudo6c())
      inFile6 <- input$file6
      if (is.null(inFile6)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      # data <- ETLEBP::cria_base_intermediaria_fapesp(origem_processos = inFile6$datapath)
      data <- ETLEBP::cria_base_intermediaria_fapesp(origem_processos = conteudo6c())

      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      # data <- ETLEBP::executa_tratamento_completo(data, fonte)
      data <- ETLEBP::executa_tratamento_completo(data, fonte)

      return(data)
    })

    #Fazer o Download
    output$download6 <- downloadHandler(
      filename = function() {
        paste("fapesp_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv2(myData6(), file, row.names = FALSE, fileEncoding = "Latin1")
      }
    )

    #CNPQ ----

    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets7c <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$file7$datapath)

      ext <- tools::file_ext(input$file7$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$file7$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$file7$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector7c <- renderUI({
      req(sheets7c())
      selectInput(ns("sheet7c"), "Escolha a aba (sheet):", choices = sheets7c()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados7c <- reactive({
      req(input$file7$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$file7$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$file7$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$file7$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$file7$datapath, sheet = input$sheet7c, n_max = 15)
      } else if (ext == "ods") {
        preview <- read_ods(input$file7$datapath, sheet = input$sheet7c, n_max = 15)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$file7$datapath, fread_args = list(skip = first_valid_row))
        }, error = function(e) {
          readr::read_csv2(input$file7$datapath, skip = first_valid_row)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$file7$datapath, sheet = input$sheet7c, skip = first_valid_row)
      } else if (ext == "ods") {
        dados <- read_ods(input$file7$datapath, sheet = input$sheet7c, skip = first_valid_row)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors7c <- renderUI({

      req(dados7c())  # Garante que os dados foram carregados
      df7c <- dados7c()

      # Nomes das colunas do dataframe
      col_names <- names(df7c)

      # Definir as variáveis esperadas
      variaveis_esperadas7c <- subset(vars, vars$Base == 'Fonte CNPQ')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas7c, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_c_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo7c <- reactive({
      req(dados7c())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas7c <- subset(vars, vars$Base == 'Fonte CNPQ')[,2]

      colunas_selecionadas7c <- sapply(variaveis_esperadas7c, function(var) {
        input[[paste0("select_c_", var)]]
      })

      # Renomear as colunas do dataframe conforme o mapeamento
      df7c <- dados7c()

      df7c <- df7c %>% rename(grande_area = colunas_selecionadas7c[[1]],
                            area = colunas_selecionadas7c[[2]],
                            subarea = colunas_selecionadas7c[[3]],
                            ano_referencia = colunas_selecionadas7c[[4]],
                            titulo_do_projeto = colunas_selecionadas7c[[5]],
                            categoria_nivel = colunas_selecionadas7c[[6]],
                            sigla_uf_destino = colunas_selecionadas7c[[7]],
                            regiao_destino = colunas_selecionadas7c[[8]],
                            pais_destino = colunas_selecionadas7c[[9]],
                            valor_pago = colunas_selecionadas7c[[10]],
                            sigla_uf_origem = colunas_selecionadas7c[[11]],
                            processo = colunas_selecionadas7c[[12]],
                            instituicao_destino = colunas_selecionadas7c[[13]])
      # colnames(df) <- colunas_selecionadas

      # Exibir o dataframe com as colunas renomeadas
      # head(df7,3)
    })
    #############################################


    myData7<- reactive({
      req(conteudo7c())
      inFile7 <- input$file7
      if (is.null(inFile7)) return(NULL)

      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      # data <- ETLEBP::cria_base_intermediaria_cnpq(origem_processos = inFile7$datapath)
      data <- ETLEBP::cria_base_intermediaria_cnpq(origem_processos = conteudo7c())

      #fonte <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      # data <- ETLEBP::executa_tratamento_completo(data, fonte)
      data <- ETLEBP::executa_tratamento_completo(data, fonte)

      return(data)
    })

    #Fazer o Download
    output$download7 <- downloadHandler(
      filename = function() {
        paste("cnpq_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData7(), file, row.names = FALSE, fileEncoding = "Latin1")
      }
    )
    #Carga incremental ----

    #ANEEL ----

    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets1 <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$i.file1$datapath)

      ext <- tools::file_ext(input$i.file1$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$i.file1$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$i.file1$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector1 <- renderUI({
      req(sheets1())
      selectInput(ns("sheet1"), "Escolha a aba (sheet):", choices = sheets1()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados1 <- reactive({
      req(input$i.file1$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$i.file1$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$i.file1$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$i.file1$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$i.file1$datapath, sheet = input$sheet1, n_max = 15, col_names = F)
      } else if (ext == "ods") {
        preview <- read_ods(input$i.file1$datapath, sheet = input$sheet1, n_max = 15, col_names = F)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$i.file1$datapath, fread_args = list(skip = first_valid_row-1), encoding = 'Latin-1')
        }, error = function(e) {
          readr::read_csv2(input$i.file1$datapath, skip = first_valid_row-1)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$i.file1$datapath, sheet = input$sheet1, skip = first_valid_row-1)
      } else if (ext == "ods") {
        dados <- read_ods(input$i.file1$datapath, sheet = input$sheet1, skip = first_valid_row-1)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors1 <- renderUI({

      req(dados1())  # Garante que os dados foram carregados
      df1 <- dados1()

      # Nomes das colunas do dataframe
      col_names <- names(df1)

      # Definir as variáveis esperadas
      variaveis_esperadas1 <- subset(vars, vars$Base == 'Fonte ANEEL')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas1, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo1 <- reactive({
      req(dados1())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas1 <- subset(vars, vars$Base == "Fonte ANEEL")[,2]

      colunas_selecionadas1 <- sapply(variaveis_esperadas1, function(var) {
        input[[paste0("select_", var)]]
      })

      # print(colunas_selecionadas)

      # Renomear as colunas do dataframe conforme o mapeamento
      df1 <- dados1()

      df1 <- df1 %>% rename(ano_cadastro_proposta_projeto = colunas_selecionadas1[[1]],
                            dat_conclusao_projeto = colunas_selecionadas1[[2]],
                            qtd_meses_duracao_prevista = colunas_selecionadas1[[3]],
                            vlr_custo_total_previsto = colunas_selecionadas1[[4]],
                            vlr_custo_total_auditado = colunas_selecionadas1[[5]],
                            dsc_titulo_projeto = colunas_selecionadas1[[6]],
                            sig_segmento_setor_eletrico = colunas_selecionadas1[[7]],
                            sig_tema_projeto = colunas_selecionadas1[[8]],
                            nom_agente = colunas_selecionadas1[[9]],
                            idc_situacao_projeto = colunas_selecionadas1[[10]],
                            cod_proj = colunas_selecionadas1[[11]],
                            sig_fas_inovacao_projeto = colunas_selecionadas1[[12]])
      # colnames(df) <- colunas_selecionadas

      # Exibir o dataframe com as colunas renomeadas
      # head(df,3)
    })
    #############################################

    i.myData <- reactive({
      req(conteudo1())
      i.inFile1 <- input$i.file1
      if (is.null(i.inFile1)) return(NULL)

      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      # data <- ETLEBP::cria_base_intermediaria_aneel(origem_processos = i.inFile1$datapath)
      data <- ETLEBP::cria_base_intermediaria_aneel(origem_processos = conteudo1())

      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      # data <- ETLEBP::executa_tratamento_incremental(data, filename)
      data <- ETLEBP::executa_tratamento_incremental(data, filename)


      return(data)
    })

    #Fazer o Download
    output$i.download <- downloadHandler(
      filename = function() {
        paste("aneel_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv2(i.myData(), file, row.names = FALSE, fileEncoding = 'Latin1', sep = ';')
      }
    )

    #ANP ----

    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets2 <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$i.file2$datapath)

      ext <- tools::file_ext(input$i.file2$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$i.file2$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$i.file2$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector2 <- renderUI({
      req(sheets2())
      selectInput(ns("sheet2"), "Escolha a aba (sheet):", choices = sheets2()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados2 <- reactive({
      req(input$i.file2$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$i.file2$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$i.file2$datapath, fread_args = list(nrows = 15),encoding = "Latin-1")
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$i.file2$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$i.file2$datapath, sheet = input$sheet2, n_max = 15, col_names = F)
      } else if (ext == "ods") {
        preview <- read_ods(input$i.file2$datapath, sheet = input$sheet2, n_max = 15, col_names = F)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$i.file2$datapath, fread_args = list(skip = first_valid_row-1),  encoding = "Latin-1")
        }, error = function(e) {
          readr::read_csv2(input$i.file2$datapath, skip = first_valid_row-1)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$i.file2$datapath, sheet = input$sheet2, skip = first_valid_row-1)
      } else if (ext == "ods") {
        dados <- read_ods(input$i.file2$datapath, sheet = input$sheet2, skip = first_valid_row-1)
      }

      return(dados)

      print(head(dados, 4))
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors2 <- renderUI({

      req(dados2())  # Garante que os dados foram carregados
      df2 <- dados2()

      # Nomes das colunas do dataframe
      col_names <- names(df2)

      # Definir as variáveis esperadas
      variaveis_esperadas2 <- subset(vars, vars$Base == 'Fonte ANP')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas2, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))
      # print(inputs)
      # print(input)

    })

    # # Mostrar os dados baseados nas seleções
    conteudo2 <- reactive({
      req(dados2())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas2 <- subset(vars, vars$Base == "Fonte ANP")[,2]

      colunas_selecionadas2 <- sapply(variaveis_esperadas2, function(var) {
        input[[paste0("select_", var)]]
      })

      # print(colunas_selecionadas)

      # Renomear as colunas do dataframe conforme o mapeamento
      df2 <- dados2()

      df2 <- df2 %>% rename(valor_clausula = colunas_selecionadas2[[1]],
                            data_inicio = colunas_selecionadas2[[2]],
                            prazo = colunas_selecionadas2[[3]],
                            titulo = colunas_selecionadas2[[4]],
                            objetivo = colunas_selecionadas2[[5]],
                            tema = colunas_selecionadas2[[6]],
                            subtema = colunas_selecionadas2[[7]],
                            no_anp = colunas_selecionadas2[[8]],
                            empresa_responsavel = colunas_selecionadas2[[9]],
                            executor = colunas_selecionadas2[[10]],
                            qualificacao = colunas_selecionadas2[[11]],
                            area = colunas_selecionadas2[[12]])

      # colnames(df) <- colunas_selecionadas
      # print(as.vector(colunas_selecionadas))

      # Exibir o dataframe com as colunas renomeadas
      # head(df,3)
    })
    #############################################


    i.myData2 <- reactive({
      req(conteudo2())
      i.inFile2 <- input$i.file2
      if (is.null(i.inFile2)) return(NULL)
      #i.inFile22 <- input$i.file22
      #if (is.null(i.inFile22)) return(NULL)

      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      # data <- ETLEBP::cria_base_intermediaria_anp(origem_processos = i.inFile2$datapath)
      data <- ETLEBP::cria_base_intermediaria_anp(origem_processos = conteudo2())


      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      # data <- ETLEBP::executa_tratamento_incremental(data, filename)
      data <- ETLEBP::executa_tratamento_incremental(data, filename)


      return(data)
    })

    #Fazer o Download
    output$i.download2 <- downloadHandler(
      filename = function() {
        paste("anp_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv2(i.myData2(), file, row.names = FALSE, fileEncoding = 'Latin1', sep = ';')
      }
    )
    #BNDES ----

    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets3 <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$i.file3$datapath)

      ext <- tools::file_ext(input$i.file3$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$i.file3$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$i.file3$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector3 <- renderUI({
      req(sheets3())
      selectInput(ns("sheet3"), "Escolha a aba (sheet):", choices = sheets3()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados3 <- reactive({
      req(input$i.file3$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$i.file3$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$i.file3$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$i.file3$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$i.file3$datapath, sheet = input$sheet3, n_max = 15, col_names = F)
      } else if (ext == "ods") {
        preview <- read_ods(input$i.file3$datapath, sheet = input$sheet3, n_max = 15, col_names = F)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$i.file3$datapath, fread_args = list(skip = first_valid_row-1), encoding = 'Latin-1')
        }, error = function(e) {
          readr::read_csv2(input$i.file3$datapath, skip = first_valid_row-1)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$i.file3$datapath, sheet = input$sheet3, skip = first_valid_row-1)
      } else if (ext == "ods") {
        dados <- read_ods(input$i.file3$datapath, sheet = input$sheet3, skip = first_valid_row-1)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors3 <- renderUI({

      req(dados3())  # Garante que os dados foram carregados
      df3 <- dados3()

      # Nomes das colunas do dataframe
      col_names <- names(df3)

      # Definir as variáveis esperadas
      variaveis_esperadas3 <- subset(vars, vars$Base == 'Fonte BNDES')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas3, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo3 <- reactive({
      req(dados3())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas3 <- subset(vars, vars$Base == "Fonte BNDES")[,2]

      colunas_selecionadas3 <- sapply(variaveis_esperadas3, function(var) {
        input[[paste0("select_", var)]]
      })

      # Renomear as colunas do dataframe conforme o mapeamento
      df3 <- dados3()

      df3 <- df3 %>% rename(inovacao = colunas_selecionadas3[[1]],
                            cnpj = colunas_selecionadas3[[2]],
                            uf = colunas_selecionadas3[[3]],
                            numero_do_contrato = colunas_selecionadas3[[4]],
                            data_da_contratacao = colunas_selecionadas3[[5]],
                            prazo_amortizacao_meses = colunas_selecionadas3[[6]],
                            prazo_carencia_meses = colunas_selecionadas3[[7]],
                            valor_contratado_r = colunas_selecionadas3[[8]],
                            produto = colunas_selecionadas3[[9]],
                            modalidade_de_apoio = colunas_selecionadas3[[10]],
                            descricao_do_projeto = colunas_selecionadas3[[11]],
                            situacao_do_contrato = colunas_selecionadas3[[12]],
                            cliente = colunas_selecionadas3[[13]],
                            natureza_do_cliente = colunas_selecionadas3[[14]])


      # colnames(df) <- colunas_selecionadas

      # Exibir o dataframe com as colunas renomeadas
      # head(df,3)
    })
    #############################################

    i.myData3 <- reactive({
      req(conteudo3())
      i.inFile3 <- input$i.file3
      if (is.null(i.inFile3)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      # data <- ETLEBP::cria_base_intermediaria_bndes(origem_processos = i.inFile3$datapath)
      data <- ETLEBP::cria_base_intermediaria_bndes(origem_processos = conteudo3())

      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      # data <- ETLEBP::executa_tratamento_incremental(data, filename)
      data <- ETLEBP::executa_tratamento_incremental(data, filename)


      return(data)
    })

    #Fazer o Download
    output$i.download3 <- downloadHandler(
      filename = function() {
        paste("bndes_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv2(i.myData3(), file, row.names = FALSE, fileEncoding = 'Latin1')
      }
    )
    #CNEN ----


    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets4 <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$i.file4$datapath)

      ext <- tools::file_ext(input$i.file4$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$i.file4$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$i.file4$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector4 <- renderUI({
      req(sheets4())
      selectInput(ns("sheet4"), "Escolha a aba (sheet):", choices = sheets4()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados4 <- reactive({
      req(input$i.file4$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$i.file4$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$i.file4$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$i.file4$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$i.file4$datapath, sheet = input$sheet4, n_max = 15, col_names = F)
      } else if (ext == "ods") {
        preview <- read_ods(input$i.file4$datapath, sheet = input$sheet4, n_max = 15, col_names = F)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$i.file4$datapath, fread_args = list(skip = first_valid_row-1))
        }, error = function(e) {
          readr::read_csv2(input$i.file4$datapath, skip = first_valid_row-1)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$i.file4$datapath, sheet = input$sheet4, skip = first_valid_row-1)
      } else if (ext == "ods") {
        dados <- read_ods(input$i.file4$datapath, sheet = input$sheet4, skip = first_valid_row-1)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors4 <- renderUI({

      req(dados4())  # Garante que os dados foram carregados
      df4 <- dados4()

      # Nomes das colunas do dataframe
      col_names <- names(df4)

      # Definir as variáveis esperadas
      variaveis_esperadas4 <- subset(vars, vars$Base == 'Fonte CNEN')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas4, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo4 <- reactive({
      req(dados4())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas4 <- subset(vars, vars$Base == "Fonte CNEN")[,2]

      colunas_selecionadas4 <- sapply(variaveis_esperadas4, function(var) {
        input[[paste0("select_", var)]]
      })

        # Renomear as colunas do dataframe conforme o mapeamento
      df4 <- dados4()
      df4 <- df4 %>% rename(data_assinatura = colunas_selecionadas4[[1]],
                            data_limite = colunas_selecionadas4[[2]],
                            categoria_da_tecnologia_digito2 = colunas_selecionadas4[[3]],
                            valor_contratado = colunas_selecionadas4[[4]],
                            titulo = colunas_selecionadas4[[5]],
                            nome_do_agente_executor = colunas_selecionadas4[[6]],
                            natureza_do_agente_financiador = colunas_selecionadas4[[7]],
                            natureza_do_financiamento = colunas_selecionadas4[[8]],
                            natureza_do_agente_executor = colunas_selecionadas4[[9]],
                            modalidade_do_financiamento = colunas_selecionadas4[[10]],
                            uf_execucao = colunas_selecionadas4[[11]],
                            p_d_ou_demonstracao = colunas_selecionadas4[[12]],
                            id = colunas_selecionadas4[[13]])
      # colnames(df) <- colunas_selecionadas4

    })
    #############################################


    i.myData4 <- reactive({
      req(conteudo4())
      i.inFile4 <- input$i.file4
      if (is.null(i.inFile4)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      # data <- ETLEBP::cria_base_intermediaria_cnen(origem_processos = i.inFile4$datapath)
      data <- ETLEBP::cria_base_intermediaria_cnen(origem_processos = conteudo4())

      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      # data <- ETLEBP::executa_tratamento_incremental(data, filename)
      data <- ETLEBP::executa_tratamento_incremental(data, filename)

      return(data)
    })

    #Fazer o Download
    output$i.download4 <- downloadHandler(
      filename = function() {
        paste("cnen_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(i.myData4(), file, row.names = FALSE, fileEncoding = 'Latin1')
      }
    )
    #FINEP ----

    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets5 <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$i.file5$datapath)

      ext <- tools::file_ext(input$i.file5$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$i.file5$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$i.file5$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector5 <- renderUI({
      req(sheets5())
      selectInput(ns("sheet5"), "Escolha a aba (sheet):", choices = sheets5()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados5 <- reactive({
      req(input$i.file5$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$i.file5$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$i.file5$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$i.file5$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$i.file5$datapath, sheet = input$sheet5, n_max = 15)
      } else if (ext == "ods") {
        preview <- read_ods(input$i.file5$datapath, sheet = input$sheet5, n_max = 15)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$i.file5$datapath, fread_args = list(skip = first_valid_row))
        }, error = function(e) {
          readr::read_csv2(input$i.file5$datapath, skip = first_valid_row)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$i.file5$datapath, sheet = input$sheet5, skip = first_valid_row)
      } else if (ext == "ods") {
        dados <- read_ods(input$i.file5$datapath, sheet = input$sheet5, skip = first_valid_row)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors5 <- renderUI({

      req(dados5())  # Garante que os dados foram carregados
      df5 <- dados5()

      # Nomes das colunas do dataframe
      col_names <- names(df5)

      # Definir as variáveis esperadas
      variaveis_esperadas5 <- subset(vars, vars$Base == 'Fonte FINEP')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas5, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo5 <- reactive({
      req(dados5())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas5 <- subset(vars, vars$Base == "Fonte FINEP")[,2]

      colunas_selecionadas5 <- sapply(variaveis_esperadas5, function(var) {
        input[[paste0("select_", var)]]
      })

      # Renomear as colunas do dataframe conforme o mapeamento
      df5 <- dados5()

      df5 <- df5 %>% rename(titulo = colunas_selecionadas5[[1]],
                            valor_finep = colunas_selecionadas5[[2]],
                            valor_liberado = colunas_selecionadas5[[3]],
                            data_assinatura = colunas_selecionadas5[[4]],
                            prazo_utilizacao = colunas_selecionadas5[[5]],
                            contrato = colunas_selecionadas5[[6]],
                            instrumento = colunas_selecionadas5[[7]],
                            proponente = colunas_selecionadas5[[8]],
                            uf = colunas_selecionadas5[[9]],
                            status = colunas_selecionadas5[[10]])

      # Exibir o dataframe com as colunas renomeadas
      # head(df,3)
    })
    #############################################

    i.myData5 <- reactive({
      req(conteudo5())
      i.inFile5 <- input$i.file5
      if (is.null(i.inFile5)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      data <- ETLEBP::cria_base_intermediaria_finep(origem_processos = conteudo5())

      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      # data <- ETLEBP::executa_tratamento_incremental(data, filename)
      data <- ETLEBP::executa_tratamento_incremental(data, filename)


      return(data)
    })

    #Fazer o Download
    output$i.download5 <- downloadHandler(
      filename = function() {
        paste("finep_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv2(i.myData5(), file, row.names = FALSE, fileEncoding = "Latin1")
      }
    )

    #FAPESP ----
    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets6 <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$i.file6$datapath)

      ext <- tools::file_ext(input$i.file6$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$i.file6$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$i.file6$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector6 <- renderUI({
      req(sheets6())
      selectInput(ns("sheet6"), "Escolha a aba (sheet):", choices = sheets6()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados6 <- reactive({
      req(input$i.file6$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$i.file6$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$i.file6$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$i.file6$datapath, n_max = 15,col_names = F)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$i.file6$datapath, sheet = input$sheet6, n_max = 15, col_names = F)
      } else if (ext == "ods") {
        preview <- read_ods(input$i.file6$datapath, sheet = input$sheet6, n_max = 15, col_names = F)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$i.file6$datapath, fread_args = list(skip = first_valid_row-1))
        }, error = function(e) {
          readr::read_csv2(input$i.file6$datapath, skip = first_valid_row-1, col_names = T)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$i.file6$datapath, sheet = input$sheet6, skip = first_valid_row-1, col_names = T)
      } else if (ext == "ods") {
        dados <- read_ods(input$i.file6$datapath, sheet = input$sheet6, skip = first_valid_row-1, col_names = T)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors6 <- renderUI({

      req(dados6())  # Garante que os dados foram carregados
      df6 <- dados6()

      # Nomes das colunas do dataframe
      col_names <- names(df6)

      # Definir as variáveis esperadas
      variaveis_esperadas6 <- subset(vars, vars$Base == 'Fonte FAPESP')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas6, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

      })

    # # Mostrar os dados baseados nas seleções
    conteudo6 <- reactive({
      req(dados6())  # Garante que os dados foram carregados
      # req(input[[paste0("select_", var)]])

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas6 <- subset(vars, vars$Base == "Fonte FAPESP")[,2]

      colunas_selecionadas6 <- sapply(variaveis_esperadas6, function(var) {
        input[[paste0("select_", var)]]
      })

      # Renomear as colunas do dataframe conforme o mapeamento
      df6 <- dados6()

      df6 <- df6 %>% rename(n_processo = colunas_selecionadas6[[1]],
                          data_de_inicio = colunas_selecionadas6[[2]],
                          data_de_termino = colunas_selecionadas6[[3]],
                          titulo_portugues = colunas_selecionadas6[[4]],
                          area_do_conhecimento = colunas_selecionadas6[[5]],
                          subarea_do_conhecimento = colunas_selecionadas6[[6]],
                          valor_concedido = colunas_selecionadas6[[7]],
                          beneficiario = colunas_selecionadas6[[8]])
      # colnames(df) <- colunas_selecionadas
      # print(colunas_selecionadas)

      # Exibir o dataframe com as colunas renomeadas
      # head(df6,3)

    })
    #############################################

    i.myData6 <- reactive({
      req(conteudo6())

      df6.1 <- conteudo6()

      i.inFile6 <- input$i.file6
      if (is.null(i.inFile6)) return(NULL)
      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      # data <- ETLEBP::cria_base_intermediaria_fapesp(origem_processos = i.inFile6$datapath)
      data <- ETLEBP::cria_base_intermediaria_fapesp(origem_processos = df6.1)


      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      # data <- ETLEBP::executa_tratamento_incremental(data, filename)
      data <- ETLEBP::executa_tratamento_incremental(data, filename)


      return(data)
    })

    #Fazer o Download
    output$i.download6 <- downloadHandler(
      filename = function() {
        paste("fapesp_interm", ".csv", sep = "")
      },
      content = function(file) {
        write.csv2(i.myData6(), file, row.names = FALSE, fileEncoding = 'Latin1')
      }
    )


    #CNPQ -----
    #############################################
    # Reativo para atualizar a lista de sheets caso seja um arquivo ODS ou XLSX

    sheets7 <- reactive({
      # inFile <- ns(input$file1)
      # if (is.null(inFile)) return(NULL)

      req(input$i.file7$datapath)

      ext <- tools::file_ext(input$i.file7$datapath)

      if (ext == "ods") {
        # Obter as sheets do arquivo ODS
        sheet_names <- ods_sheets(input$i.file7$datapath)
      } else if (ext == "xlsx") {
        # Obter as sheets do arquivo XLSX
        sheet_names <- excel_sheets(input$i.file7$datapath)
      } else {
        sheet_names <- NULL
      }

      return(sheet_names)
    })

    # UI dinâmico para seleção de sheets
    output$sheet_selector7 <- renderUI({
      req(sheets7())
      selectInput(ns("sheet7"), "Escolha a aba (sheet):", choices = sheets7()
                  # , selected = sheets1()[1]
      )
    })



    # Função para identificar a primeira linha sem NA no dataframe
    find_first_valid_row <- function(df) {
      for (i in 1:nrow(df)) {
        row_data <- df[i, ]
        if (all(!is.na(row_data))) {  # Se a linha não tiver NA
          return(i)
        }
      }
      return(NA)  # Se não encontrar nenhuma linha válida
    }

    # Função reativa para ler o arquivo e encontrar a primeira linha válida
    dados7 <- reactive({
      req(input$i.file7$datapath)      # Garante que um arquivo foi carregado

      ext <- tools::file_ext(input$i.file7$datapath)

      # Ler as primeiras linhas para identificar a linha de início
      if (ext == "csv") {
        # Tentar ler o CSV com vírgula como separador
        preview <- tryCatch({
          rio::import(input$i.file7$datapath, fread_args = list(nrows = 15))
        }, error = function(e) {
          # Se falhar, tentar com ponto e vírgula como separador
          readr::read_csv2(input$i.file7$datapath, n_max = 15)
        })
      } else if (ext == "xlsx") {
        preview <- readxl::read_excel(input$i.file7$datapath, sheet = input$sheet7, n_max = 15)
      } else if (ext == "ods") {
        preview <- read_ods(input$i.file7$datapath, sheet = input$sheet7, n_max = 15)
      } else {
        stop("Extensão de arquivo não suportada. Use '.csv', '.xlsx' ou '.ods'.")
      }

      # Encontrar a primeira linha sem NA
      first_valid_row <- find_first_valid_row(preview)
      if (is.na(first_valid_row)) {
        stop("Nenhuma linha válida encontrada no arquivo.")
      }

      # Ler o arquivo completo a partir da linha válida identificada
      if (ext == "csv") {
        dados <- tryCatch({
          rio::import(input$i.file7$datapath, fread_args = list(skip = first_valid_row))
        }, error = function(e) {
          readr::read_csv2(input$i.file7$datapath, skip = first_valid_row)
        })
      } else if (ext == "xlsx") {
        dados <- readxl::read_excel(input$i.file7$datapath, sheet = input$sheet7, skip = first_valid_row)
      } else if (ext == "ods") {
        dados <- read_ods(input$i.file7$datapath, sheet = input$sheet7, skip = first_valid_row)
      }

      return(dados)
    })

    # Gerar os inputs de seleção dinamicamente
    output$var_selectors7 <- renderUI({

      req(dados7())  # Garante que os dados foram carregados
      df7 <- dados7()

      # Nomes das colunas do dataframe
      col_names <- names(df7)

      # Definir as variáveis esperadas
      variaveis_esperadas <- subset(vars, vars$Base == 'Fonte CNPQ')[,2]

      # Criar um selectInput para cada variável esperada dentro de colunas para disposição lado a lado
      inputs <- lapply(variaveis_esperadas, function(var) {
        column(3,  # Ajuste a largura de cada coluna (total de 12 colunas na `fluidRow`)
               selectInput(
                 inputId = ns(paste0("select_", var)),
                 label = paste("Selecione a coluna para", var, ":"),
                 choices = c("Escolha uma opção" = '', col_names),
                 selected = NULL
               ))
      })

      # Exibir os selectInput em uma única linha
      fluidRow(do.call(tagList, inputs))

    })

    # # Mostrar os dados baseados nas seleções
    conteudo7 <- reactive({
      req(dados7())  # Garante que os dados foram carregados

      # Mapeamento das variáveis esperadas para as colunas selecionadas
      variaveis_esperadas7 <- subset(vars, vars$Base == 'Fonte CNPQ')[,2]

      colunas_selecionadas7 <- sapply(variaveis_esperadas7, function(var) {
        input[[paste0("select_", var)]]
      })

      # Renomear as colunas do dataframe conforme o mapeamento
      df7 <- dados7()

      df7 <- df7 %>% rename(grande_area = colunas_selecionadas7[[1]],
                            area = colunas_selecionadas7[[2]],
                            subarea = colunas_selecionadas7[[3]],
                            ano_referencia = colunas_selecionadas7[[4]],
                            titulo_do_projeto = colunas_selecionadas7[[5]],
                            categoria_nivel = colunas_selecionadas7[[6]],
                            sigla_uf_destino = colunas_selecionadas7[[7]],
                            regiao_destino = colunas_selecionadas7[[8]],
                            pais_destino = colunas_selecionadas7[[9]],
                            valor_pago = colunas_selecionadas7[[10]],
                            sigla_uf_origem = colunas_selecionadas7[[11]],
                            processo = colunas_selecionadas7[[12]],
                            instituicao_destino = colunas_selecionadas7[[13]])
      # colnames(df) <- colunas_selecionadas

      # Exibir o dataframe com as colunas renomeadas
      # head(df7,3)
    })
    #############################################

    i.myData7 <- reactive({
      req(conteudo7())
      i.inFile7 <- input$i.file7
      if (is.null(i.inFile7)) return(NULL)

      #data <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
      # data <- ETLEBP::cria_base_intermediaria_cnpq(origem_processos = i.inFile7 )
      data <- ETLEBP::cria_base_intermediaria_cnpq(origem_processos = conteudo7())

      #Criando dataset com casos novos
      #filename <- "data/DB_EIP/EIP_20210415.db"
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      filename <- filesqlite$datapath

      # data <- ETLEBP::executa_tratamento_incremental(data, filename)
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
        write.csv(i.myData7(), file, row.names = FALSE, fileEncoding = 'Latin1')
      }
    )

  })
}

## To be copied in the UI
# mod_tratamento_ui("tratamento_1")

## To be copied in the server
# mod_tratamento_server("tratamento_1")
