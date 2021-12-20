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
 shiny::actionButton(ns("carga1"), "Executar Carregamento Completo no SQLite"),
 shiny::actionButton(ns("carga2"), "Executar Carregamento Incremental no SQLite"), width = 10))


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


    observeEvent(input$carga1, {

      #identificando o diretorio do .db
      filename <- "data/DB_EIP/EIP_20210415.db"
      con<-DBI::dbConnect(RSQLite::SQLite(),
                          dbname="myDB")

      #estabelecendo conexão
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)

      mytbl1 <- DBI::dbReadTable(con,"dm_agente_empresa")
      mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
      mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

      inicio<-(max(mytbl1$id_agente)+1)

      fim<-(inicio+nrow(data())-1)

      dm_agente_empresa <- data() %>%
        dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4,
                                     2.1, 2.2, 2.3,
                                     3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7,
                                     4.1, 4.2,
                                     5.1, 5.2,
                                     6.1, 6.2, 6.3,
                                     7.1, 7.2) ) %>%

        dplyr::select(nome_agente_executor,
                      nome_agente_financiador,
                      uf_ag_executor,
                      fonte_dados,
                      natureza_agente_executor)

      dm_agente_empresa <- dm_agente_empresa %>%
        dplyr::mutate(
          id_agente = inicio:fim,
          nme_agente = nome_agente_executor,
          uf = uf_ag_executor,
          municipio = NA,
          cnpj = NA,
          ntz_agente = natureza_agente_executor
        ) %>%
        dplyr::select(id_agente,
                      nme_agente,
                      ntz_agente,
                      uf,
                      municipio,
                      cnpj)

      DBI::dbExecute(con, 'INSERT INTO dm_agente_empresa (id_agente, nme_agente,ntz_agente, uf, municipio, cnpj)
          VALUES (:id_agente, :nme_agente, :ntz_agente, :uf, :municipio, :cnpj);', dm_agente_empresa)

      DBI::dbDisconnect(con)
    })



    observeEvent(input$carga1, {

      #identificando o diretorio do .db
      filename <- "data/DB_EIP/EIP_20210415.db"

      #con<-DBI::dbConnect(RSQLite::SQLite(),dbname="myDB")

      #estabelecendo conexão
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)

      mytbl6 <- DBI::dbReadTable(con,"dm_projeto")

      #Carga dm_projeto
      dm_projeto <- data() %>%
        dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4,
                                        2.1, 2.2, 2.3,
                                        3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7,
                                        4.1, 4.2,
                                        5.1, 5.2,
                                        6.1, 6.2, 6.3,
                                        7.1, 7.2) ) %>%
        dplyr::select(id, data_assinatura,
                      data_limite,titulo_projeto,status_projeto)


      inicio<-(max(mytbl6$id_projeto)+1)

      fim<-(inicio+nrow(data())-1)

      dm_projeto <- dm_projeto %>%
        dplyr::mutate(id_projeto = inicio:fim,
                      id_item = id,
                      dta_inicio = as.character(data_assinatura),
                      dta_limite = as.character(data_limite),
                      'título'     = titulo_projeto,
                      'situação'   = status_projeto) %>%
        dplyr::select(id_projeto,id_item,
                      dta_inicio,dta_limite,'título',
                      'situação')


      DBI::dbExecute(con, 'INSERT INTO dm_projeto (id_projeto, id_item,dta_inicio, dta_limite, título, situação)
          VALUES (:id_projeto, :id_item, :dta_inicio, :dta_limite, :título, :situação);', dm_projeto)
      DBI::dbDisconnect(con)
    })

    observeEvent(input$carga1,{
      #Carga ft_dispendio

      #identificando o diretorio do .db
      filename <- "data/DB_EIP/EIP_20210415.db"
      #con<-DBI::dbConnect(RSQLite::SQLite(),dbname="myDB")

      #estabelecendo conexão
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)

      mytbl1 <- DBI::dbReadTable(con,"dm_agente_empresa")
      mytbl2 <- dbReadTable(con,"dm_categoria")
      mytbl3 <- dbReadTable(con,"dm_formentador")
      mytbl4 <- dbReadTable(con,"dm_mod_finan")
      mytbl5 <- dbReadTable(con,"dm_nat_disp")
      mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
      mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

      vlr_res <- data() %>%
        dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4,
                                        2.1, 2.2, 2.3,
                                        3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7,
                                        4.1, 4.2,
                                        5.1, 5.2,
                                        6.1, 6.2, 6.3,
                                        7.1, 7.2) ) %>%
        dplyr::select(id, valor_executado_2013:valor_executado_2020) %>%
        tidyr::gather(ano, vlr, -id) %>%
        dplyr::mutate(ano = dplyr::recode(ano,
                                   "valor_executado_2013" = 2013,
                                   "valor_executado_2014" = 2014,
                                   "valor_executado_2015" = 2015,
                                   "valor_executado_2016" = 2016,
                                   "valor_executado_2017" = 2017,
                                   "valor_executado_2018" = 2018,
                                   "valor_executado_2019" = 2019,
                                   "valor_executado_2020" = 2020)) %>%
        dplyr::rename(id_item = id)


      bs_res <- data() %>%
        dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4,
                                        2.1, 2.2, 2.3,
                                        3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7,
                                        4.1, 4.2,
                                        5.1, 5.2,
                                        6.1, 6.2, 6.3,
                                        7.1, 7.2) ) %>%
        dplyr::select(id, natureza_agente_financiador,
                      data_assinatura,categorias,nome_agente_executor,
                      fonte_dados, modalidade_financiamento)

      outra<- bs_res %>% dplyr::select(nome_agente_executor)%>%
        na.omit(nome_agente_executor)
      outra<- dplyr::left_join(outra, mytbl1[,c(1,2)],
                               by = c("nome_agente_executor"="nme_agente"))%>%
        dplyr::rename(id_exec = id_agente)

      bs_res <- dplyr::left_join(bs_res, outra) %>% unique()


      bs_res <- dplyr::left_join(bs_res, mytbl2[,c(1,3)],
                                 by =  c("categorias" = "cat2")) %>%
        dplyr::rename(id_item = id.x,
                      id_cat2 = id.y,
                      dta_inicio = data_assinatura)


      inicio<-(max(mytbl7$id_disp)+1)

      fim<-(inicio+nrow(bs_res)-1)
      bs_res<-bs_res %>% dplyr::mutate(fonte_dados = dplyr::recode(fonte_dados,
                                                                   "ANEEL"  = 5,
                                                                   "BNDES"  = 6,
                                                                   "FINEP"  = 7,
                                                                   "Finep"  = 7,
                                                                   "FNDCT"  = 8,
                                                                   "CNPq"   = 9,
                                                                   "FAPESP" = 10,
                                                                   "ANP"    = 11,
                                                                   "CNEN"   = 12),
                                       modalidade_financiamento = dplyr::recode(modalidade_financiamento,
                                                                                "Reembolsável"  = 1,
                                                                                "Não-reembolsável"= 2,
                                                                                "NÃO REEMBOLSÁVEL" =2,
                                                                                "Não Reembolsável" =2,
                                                                                "REEMBOLSÁVEL" = 1,
                                                                                "Subvenção" =3,
                                                                                "Não se Aplica" = 4,
                                                                                "Não informado" =5),
                                       natureza_agente_financiador = dplyr::recode(natureza_agente_financiador,
                                                                                   "Empresa Privada" = 0,
                                                                                   "empresa pública" = 1,
                                                                                   "Empresa pública" = 1,
                                                                                   "Empresa Pública" = 1,
                                                                                   "Empresa privada" = 0,
                                                                                   "Empresa economia mista" =1,
                                                                                   "Fundação de Amparo (FAP)" = 1,
                                                                                   "ICT pública" =1,
                                                                                   "ONU" =0),
                                       chamada = NA,
                                       id_disp = inicio:fim
      ) %>%
        dplyr::rename(id_formnt = fonte_dados,
                      mod_finan = modalidade_financiamento,
                      ntz_finan = natureza_agente_financiador)
      #id_prop e id_finan e id_exec medem a mesma coisa
      bs_res<-dplyr::left_join(vlr_res, bs_res )
      bs_res<-bs_res %>% dplyr::select(-nome_agente_executor,-categorias)

      DBI::dbExecute(con, 'INSERT INTO ft_dispendio (id_item, ano, vlr, ntz_finan, dta_inicio,
                                          id_exec, id_formnt, mod_finan, id_cat2,chamada, id_disp)
          VALUES (:id_item, :ano, :vlr, :ntz_finan, :dta_inicio,
                  :id_exec, :id_formnt, :mod_finan, :id_cat2, :chamada, :id_disp);', bs_res)

      DBI::dbDisconnect(con)
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


    observeEvent(input$carga2, {

      #identificando o diretorio do .db
      filename <- "data/DB_EIP/EIP_20210415.db"
      con<-DBI::dbConnect(RSQLite::SQLite(),
                          dbname="myDB")

      #estabelecendo conexão
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)

      mytbl1 <- DBI::dbReadTable(con,"dm_agente_empresa")
      mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
      mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

      inicio<-(max(mytbl1$id_agente)+1)

      fim<-(inicio+nrow(data())-1)

      dm_agente_empresa <- data() %>%
        dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4,
                                        2.1, 2.2, 2.3,
                                        3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7,
                                        4.1, 4.2,
                                        5.1, 5.2,
                                        6.1, 6.2, 6.3,
                                        7.1, 7.2),
                      !titulo_projeto %in% mytbl6$título) %>%

        dplyr::select(nome_agente_executor,
                      nome_agente_financiador,
                      uf_ag_executor,
                      fonte_dados,
                      natureza_agente_executor)

      dm_agente_empresa <- dm_agente_empresa %>%
        dplyr::mutate(
          id_agente = inicio:fim,
          nme_agente = nome_agente_executor,
          uf = uf_ag_executor,
          municipio = NA,
          cnpj = NA,
          ntz_agente = natureza_agente_executor
        ) %>%
        dplyr::select(id_agente,
                      nme_agente,
                      ntz_agente,
                      uf,
                      municipio,
                      cnpj)

      DBI::dbExecute(con, 'INSERT INTO dm_agente_empresa (id_agente, nme_agente,ntz_agente, uf, municipio, cnpj)
          VALUES (:id_agente, :nme_agente, :ntz_agente, :uf, :municipio, :cnpj);', dm_agente_empresa)

      DBI::dbDisconnect(con)
    })



    observeEvent(input$carga2, {

      #identificando o diretorio do .db
      filename <- "data/DB_EIP/EIP_20210415.db"

      #con<-DBI::dbConnect(RSQLite::SQLite(),dbname="myDB")

      #estabelecendo conexão
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)

      mytbl6 <- DBI::dbReadTable(con,"dm_projeto")

      #Carga dm_projeto
      dm_projeto <- data() %>%
        dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4,
                                        2.1, 2.2, 2.3,
                                        3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7,
                                        4.1, 4.2,
                                        5.1, 5.2,
                                        6.1, 6.2, 6.3,
                                        7.1, 7.2),
                      !titulo_projeto %in% mytbl6$título) %>%
        dplyr::select(id, data_assinatura,
                      data_limite,titulo_projeto,status_projeto)


      inicio<-(max(mytbl6$id_projeto)+1)

      fim<-(inicio+nrow(data())-1)

      dm_projeto <- dm_projeto %>%
        dplyr::mutate(id_projeto = inicio:fim,
                      id_item = id,
                      dta_inicio = as.character(data_assinatura),
                      dta_limite = as.character(data_limite),
                      'título'     = titulo_projeto,
                      'situação'   = status_projeto) %>%
        dplyr::select(id_projeto,id_item,
                      dta_inicio,dta_limite,'título',
                      'situação')


      DBI::dbExecute(con, 'INSERT INTO dm_projeto (id_projeto, id_item,dta_inicio, dta_limite, título, situação)
          VALUES (:id_projeto, :id_item, :dta_inicio, :dta_limite, :título, :situação);', dm_projeto)
      DBI::dbDisconnect(con)
    })


    #Atualizando Status do Projeto
    observeEvent(input$carga2,{
      filename <- "data/DB_EIP/EIP_20210415.db"

      #estabelecendo conexão
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)

      a<- data()

      dbExecute(con, "UPDATE dm_projeto SET situação = :situação where id_item = :id_item",
                params=data.frame(situação=a$status_projeto,
                                  id_item=a$id))
      DBI::dbDisconnect(con)
    })

    observeEvent(input$carga2,{

      #Carga ft_dispendio

      #identificando o diretorio do .db
      filename <- "data/DB_EIP/EIP_20210415.db"
      #con<-DBI::dbConnect(RSQLite::SQLite(),dbname="myDB")

      #estabelecendo conexão
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = filename)

      mytbl1 <- DBI::dbReadTable(con,"dm_agente_empresa")
      mytbl2 <- dbReadTable(con,"dm_categoria")
      mytbl3 <- dbReadTable(con,"dm_formentador")
      mytbl4 <- dbReadTable(con,"dm_mod_finan")
      mytbl5 <- dbReadTable(con,"dm_nat_disp")
      mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
      mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

      vlr_res <- data() %>%
        dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4,
                                        2.1, 2.2, 2.3,
                                        3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7,
                                        4.1, 4.2,
                                        5.1, 5.2,
                                        6.1, 6.2, 6.3,
                                        7.1, 7.2),
                      !titulo_projeto %in% mytbl6$título) %>%
        dplyr::select(id, valor_executado_2013:valor_executado_2020) %>%
        tidyr::gather(ano, vlr, -id) %>%
        dplyr::mutate(ano = dplyr::recode(ano,
                                          "valor_executado_2013" = 2013,
                                          "valor_executado_2014" = 2014,
                                          "valor_executado_2015" = 2015,
                                          "valor_executado_2016" = 2016,
                                          "valor_executado_2017" = 2017,
                                          "valor_executado_2018" = 2018,
                                          "valor_executado_2019" = 2019,
                                          "valor_executado_2020" = 2020)) %>%
        dplyr::rename(id_item = id)


      bs_res <- data() %>%
        dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4,
                                        2.1, 2.2, 2.3,
                                        3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7,
                                        4.1, 4.2,
                                        5.1, 5.2,
                                        6.1, 6.2, 6.3,
                                        7.1, 7.2),
                      !titulo_projeto %in% mytbl6$título) %>%
        dplyr::select(id, natureza_agente_financiador,
                      data_assinatura,categorias,nome_agente_executor,
                      fonte_dados, modalidade_financiamento)

      outra<- bs_res %>% dplyr::select(nome_agente_executor)%>%
        na.omit(nome_agente_executor)
      outra<- dplyr::left_join(outra, mytbl1[,c(1,2)],
                               by = c("nome_agente_executor"="nme_agente"))%>%
        dplyr::rename(id_exec = id_agente)

      bs_res <- dplyr::left_join(bs_res, outra) %>% unique()


      bs_res <- dplyr::left_join(bs_res, mytbl2[,c(1,3)],
                                 by =  c("categorias" = "cat2")) %>%
        dplyr::rename(id_item = id.x,
                      id_cat2 = id.y,
                      dta_inicio = data_assinatura)


      inicio<-(max(mytbl7$id_disp)+1)

      fim<-(inicio+nrow(bs_res)-1)
      bs_res<-bs_res %>% dplyr::mutate(fonte_dados = dplyr::recode(fonte_dados,
                                                                   "ANEEL"  = 5,
                                                                   "BNDES"  = 6,
                                                                   "FINEP"  = 7,
                                                                   "Finep"  = 7,
                                                                   "FNDCT"  = 8,
                                                                   "CNPq"   = 9,
                                                                   "FAPESP" = 10,
                                                                   "ANP"    = 11,
                                                                   "CNEN"   = 12),
                                       modalidade_financiamento = dplyr::recode(modalidade_financiamento,
                                                                                "Reembolsável"  = 1,
                                                                                "Não-reembolsável"= 2,
                                                                                "NÃO REEMBOLSÁVEL" =2,
                                                                                "Não Reembolsável" =2,
                                                                                "REEMBOLSÁVEL" = 1,
                                                                                "Subvenção" =3,
                                                                                "Não se Aplica" = 4,
                                                                                "Não informado" =5),
                                       natureza_agente_financiador = dplyr::recode(natureza_agente_financiador,
                                                                                   "Empresa Privada" = 0,
                                                                                   "empresa pública" = 1,
                                                                                   "Empresa pública" = 1,
                                                                                   "Empresa Pública" = 1,
                                                                                   "Empresa privada" = 0,
                                                                                   "Empresa economia mista" =1,
                                                                                   "Fundação de Amparo (FAP)" = 1,
                                                                                   "ICT pública" =1,
                                                                                   "ONU" =0),
                                       chamada = NA,
                                       id_disp = inicio:fim
      ) %>%
        dplyr::rename(id_formnt = fonte_dados,
                      mod_finan = modalidade_financiamento,
                      ntz_finan = natureza_agente_financiador)
      #id_prop e id_finan e id_exec medem a mesma coisa
      bs_res<-dplyr::left_join(vlr_res, bs_res )
      bs_res<-bs_res %>% dplyr::select(-nome_agente_executor,-categorias)

      DBI::dbExecute(con, 'INSERT INTO ft_dispendio (id_item, ano, vlr, ntz_finan, dta_inicio,
                                          id_exec, id_formnt, mod_finan, id_cat2,chamada, id_disp)
          VALUES (:id_item, :ano, :vlr, :ntz_finan, :dta_inicio,
                  :id_exec, :id_formnt, :mod_finan, :id_cat2, :chamada, :id_disp);', bs_res)

      DBI::dbDisconnect(con)
    })

    #Mandar msg p usuário
    observeEvent(input$carga2, {
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
