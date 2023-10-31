#' status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList
#' @import bslib
#' @import plotly
#' @import RSQLite
#' @import DT

mod_status_ui <- function(id){
  ns <- NS(id)
  page_sidebar(
    theme = bs_theme(bootswatch = "minty"),
    sidebar = sidebar(
      tagList( actionLink(ns("selectall"),"Selecionar Todos"),
               checkboxGroupInput(
                 ns("fomentos"), "Fomentador:",
                 choices = c("ANEEL", "ANP","BNDES","CNEN","CNPq","FAPESP","FINEP"))
      ),
      fileInput(ns("file_sqlite"), "Indique o diretório da base SQLite (Atual)"),
      fileInput(ns("file_sqlite2"), "Indique o diretório da base SQLite (Novo)")
    ),
    fluidRow(
    mainPanel(
      tabsetPanel(
        tabPanel("Atual",DT::DTOutput(ns("ini"))),
        tabPanel("Valor",plotly::plotlyOutput(ns("graph1"))),
        tabPanel("Novos",plotly::plotlyOutput(ns("graph2"))),
        tabPanel("Categorias",plotly::plotlyOutput(ns("graph5"))),
        tabPanel("Busca",DT::DTOutput(ns("tab1")))

      )
    )
    ),
    fluidRow(
      mainPanel(
        tabsetPanel(
          tabPanel("Novo",DT::DTOutput(ns("ini2"))),
          tabPanel("Valor",plotly::plotlyOutput(ns("graph12"))),
          tabPanel("Novos",plotly::plotlyOutput(ns("graph22"))),
          tabPanel("Categorias",plotly::plotlyOutput(ns("graph52"))),
          tabPanel("Busca",DT::DTOutput(ns("tab12")))

        )
      )
    )
  )
}

#' status Server Functions
#'
#' @noRd
mod_status_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
############### TABELA TRATADA PARA UTILIZAÇÃO DE GRÁFICOS E TABELAS (ATUAL)
    myData_etl <- reactive({
      filesqlite<- input$file_sqlite
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = fonte)


      tbl_dm_categoria <- DBI::dbReadTable(con,"dm_categoria")
      tbl_dm_fmt <- DBI::dbReadTable(con,"dm_formentador")
      tbl_mod_finan <- DBI::dbReadTable(con,"dm_mod_finan")
      tbl_nat_disp <- DBI::dbReadTable(con,"dm_nat_disp")
      tbl_dm_projeto <- DBI::dbReadTable(con,"dm_projeto")
      tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")
      tbl_ft_projeto <- dbReadTable(con, "dm_projeto")

      dm_projeto <- tbl_dm_projeto %>% dplyr::distinct(id_item,dta_inicio)

      tabela <- merge(tbl_ft_dispendio,tbl_dm_fmt[,c("id_formentador","nme_form")],by.x = "id_formnt",by.y="id_formentador")
      tabela <- merge(tabela,tbl_dm_categoria,by.x = "id_cat2",by.y = "id",all.x = T)
      tabela <- merge(tabela,dm_projeto[,c("id_item","dta_inicio")], by ="id_item",all.x = T)
      tabela$ano_inicio <- lubridate::year(tabela$dta_inicio.y)
      tabela <- subset(tabela,tabela$ano_inicio >= 2013)

    })

################# ETL PARA GRÁFICO DA ABA VALOR (ATUAL)
    myData <- reactive({
    tabela <- myData_etl()
    tabela <- tabela[,c("ano","nme_form","vlr")]
    tabela <- aggregate(vlr ~ ano*nme_form, tabela, sum)
    tabela <- reshape(tabela, idvar = "ano", timevar = "nme_form", direction = "wide")
    tabela <- rename(tabela,c( "Ano" = "ano",
                               "ANEEL" = "vlr.ANEEL",
                               "ANP" = "vlr.ANP",
                               "BNDES" = "vlr.BNDES",
                               "CNEN" = "vlr.CNEM",
                               "CNPq" ="vlr.CNPq",
                               "FAPESP" = "vlr.FAPESP",
                               "FINEP" ="vlr.FINEP"))

    req(input$fomentos)
    tabela <- tabela |> select("Ano",input$fomentos)
    tabela
    })

############# ETL PARA GRÁFICO DA ABA NOVOS (ATUAL)
    myData2 <- reactive({
      tabela <- myData_etl()
      tabela <- tabela[,c("ano_inicio","nme_form")]
      tabela <- tabela %>% rename( ano = ano_inicio)
      tabela <- data.frame(tabela,contador=1)
      tabela <- aggregate(contador ~ ano*nme_form, tabela, sum)
      tabela <- reshape(tabela, idvar = "ano", timevar = "nme_form", direction = "wide")

      tabela[is.na(tabela)] <- 0

      tabela = rename(tabela,c( "Ano" = "ano",
                                                            "ANEEL" = "contador.ANEEL",
                                                            "ANP" = "contador.ANP",
                                                            "BNDES" = "contador.BNDES",
                                                            "CNEN" = "contador.CNEM",
                                                            "CNPq" ="contador.CNPq",
                                                            "FAPESP" = "contador.FAPESP",
                                                            "FINEP" ="contador.FINEP"))

      req(input$fomentos)
      tabela <- tabela |> select("Ano",input$fomentos)
      tabela
    })

############# ETL PARA GRÁFICO DA ABA CATEGORIAS (ATUAL)
    myData3 <- reactive({
      tabela <- myData_etl()
      tabela <- tabela[,c("nme_form","cat1","ano")]
      tabela <- data.frame(tabela,contador=1)
      tabela <- aggregate(contador ~ ano*nme_form*cat1, tabela, sum)
      tabela$nme_form[tabela$nme_form=="CNEM"] <- "CNEN"
      tabela <- reshape(tabela, idvar = c("ano","nme_form"), timevar = "cat1", direction = "wide")
      tabela <- tabela %>% rename( Ano = ano)
      colnames(tabela) <- c("Ano","nme_form","Cat_1","Cat_2","Cat_3","Cat_4",
                                          "Cat_5","Cat_6","Cat_7")

      req(input$fomentos)
      tabela <- tabela |> filter(nme_form %in% c(input$fomentos))
      tabela
    })

############# GRÁFICO DA ABA VALOR (ATUAL)
    output$graph1 <-
      plotly::renderPlotly({
         datag <- myData()
       colNames <- names(datag)[-1]
       p <- plotly::plot_ly(data = datag,
                            x = ~Ano,
                            y = as.formula(paste0("~", colNames[1])),
                            type = "bar",
                            name = paste0(colNames[1])) %>%
         layout(yaxis = list(title = 'Valor'), barmode = 'stack',title="Valor dos Projetos por Ano e Fomentador")

       for(trace in colNames[-1]){
         p <- p %>% plotly::add_trace(y = as.formula(paste0("~", trace)), name = trace)
       }
       p
     })

############# GRÁFICO DA ABA NOVOS (ATUAL)
    output$graph2 <-
      plotly::renderPlotly({
        datag2 <- myData2()
        colNames <- names(datag2)[-1]
        p <- plotly::plot_ly(data = datag2,
                             x = ~Ano,
                             y = as.formula(paste0("~", colNames[1])),
                             type = "bar",
                             name = paste0(colNames[1])) %>%
          layout(yaxis = list(title = 'Número'), barmode = 'stack',title="Número de Projetos Novos por Ano e Fomentador")

        for(trace in colNames[-1]){
          p <- p %>% plotly::add_trace(y = as.formula(paste0("~", trace)), name = trace)
        }
        p
      })

############# GRÁFICO DA ABA CATEGORIAS (ATUAL)
    output$graph5 <-
      plotly::renderPlotly({
        datag3 <- myData3()
        colNames <- names(datag3)[-c(1:2)]
        p <- plotly::plot_ly(data = datag3,
                             x = ~Ano,
                             y = as.formula(paste0("~", colNames[1])),
                             type = "bar",
                             name = paste0(colNames[1])) %>%
          layout(yaxis = list(title = 'Número'), barmode = 'stack',title="Número de Categorias Nível 1 por Ano")

        for(trace in colNames[-1]){
          p <- p %>% plotly::add_trace(y = as.formula(paste0("~", trace)), name = trace)
        }
        p
      })

############# TABELA DE BUSCA (ATUAL)
    output$tab1 <-
      DT::renderDT({
        datag0 <- myData_etl()
        DT::datatable(datag0,
                  filter = 'top',
                  rownames = FALSE,
                  options = list(
                    scrollX = TRUE,
                    searching = TRUE,
                    pageLength = 10,
                    columnDefs = list(list(
                      targets = "_all",
                      render = DT::JS(
                        "function(data, type, row, meta) {",
                        "return type === 'display' && data != null && data.length > 15 ?",
                        "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                        "}"
                      )
                    ))
                  ))
      })

############### TABELA TRATADA PARA UTILIZAÇÃO DE GRÁFICOS E TABELAS (NOVO)
    myData_etl2 <- reactive({
      filesqlite<- input$file_sqlite2
      if (is.null(filesqlite)) return(NULL)
      fonte <- filesqlite$datapath

      con <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:",
                            dbname = fonte)


      tbl_dm_categoria <- DBI::dbReadTable(con,"dm_categoria")
      tbl_dm_fmt <- DBI::dbReadTable(con,"dm_formentador")
      tbl_mod_finan <- DBI::dbReadTable(con,"dm_mod_finan")
      tbl_nat_disp <- DBI::dbReadTable(con,"dm_nat_disp")
      tbl_dm_projeto <- DBI::dbReadTable(con,"dm_projeto")
      tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")
      tbl_ft_projeto <- dbReadTable(con, "dm_projeto")

      dm_projeto <- tbl_dm_projeto %>% dplyr::distinct(id_item,dta_inicio)

      tabela <- merge(tbl_ft_dispendio,tbl_dm_fmt[,c("id_formentador","nme_form")],by.x = "id_formnt",by.y="id_formentador")
      tabela <- merge(tabela,tbl_dm_categoria,by.x = "id_cat2",by.y = "id",all.x = T)
      tabela <- merge(tabela,dm_projeto[,c("id_item","dta_inicio")], by ="id_item",all.x = T)
      tabela$ano_inicio <- lubridate::year(tabela$dta_inicio.y)
      tabela <- subset(tabela,tabela$ano_inicio >= 2013)

    })

################# ETL PARA GRÁFICO DA ABA VALOR (NOVO)
    myDatan <- reactive({
      tabela <- myData_etl2()
      tabela <- tabela[,c("ano","nme_form","vlr")]
      tabela <- aggregate(vlr ~ ano*nme_form, tabela, sum)
      tabela <- reshape(tabela, idvar = "ano", timevar = "nme_form", direction = "wide")
      tabela <- rename(tabela,c( "Ano" = "ano",
                                 "ANEEL" = "vlr.ANEEL",
                                 "ANP" = "vlr.ANP",
                                 "BNDES" = "vlr.BNDES",
                                 "CNEN" = "vlr.CNEM",
                                 "CNPq" ="vlr.CNPq",
                                 "FAPESP" = "vlr.FAPESP",
                                 "FINEP" ="vlr.FINEP"))

      req(input$fomentos)
      tabela <- tabela |> select("Ano",input$fomentos)
      tabela
    })

############# ETL PARA GRÁFICO DA ABA NOVOS (NOVO)
    myData2n <- reactive({
      tabela <- myData_etl2()
      tabela <- tabela[,c("ano_inicio","nme_form")]
      tabela <- tabela %>% rename( ano = ano_inicio)
      tabela <- data.frame(tabela,contador=1)
      tabela <- aggregate(contador ~ ano*nme_form, tabela, sum)
      tabela <- reshape(tabela, idvar = "ano", timevar = "nme_form", direction = "wide")

      tabela[is.na(tabela)] <- 0

      tabela = rename(tabela,c( "Ano" = "ano",
                                "ANEEL" = "contador.ANEEL",
                                "ANP" = "contador.ANP",
                                "BNDES" = "contador.BNDES",
                                "CNEN" = "contador.CNEM",
                                "CNPq" ="contador.CNPq",
                                "FAPESP" = "contador.FAPESP",
                                "FINEP" ="contador.FINEP"))

      req(input$fomentos)
      tabela <- tabela |> select("Ano",input$fomentos)
      tabela
    })

############# ETL PARA GRÁFICO DA ABA CATEGORIAS (NOVO)
    myData3n <- reactive({
      tabela <- myData_etl2()
      tabela <- tabela[,c("nme_form","cat1","ano")]
      tabela <- data.frame(tabela,contador=1)
      tabela <- aggregate(contador ~ ano*nme_form*cat1, tabela, sum)
      tabela$nme_form[tabela$nme_form=="CNEM"] <- "CNEN"
      tabela <- reshape(tabela, idvar = c("ano","nme_form"), timevar = "cat1", direction = "wide")
      tabela <- tabela %>% rename( Ano = ano)
      colnames(tabela) <- c("Ano","nme_form","Cat_1","Cat_2","Cat_3","Cat_4",
                            "Cat_5","Cat_6","Cat_7")

      req(input$fomentos)
      tabela <- tabela |> filter(nme_form %in% c(input$fomentos))
      tabela
    })

############# GRÁFICO DA ABA VALOR (NOVO)
    output$graph12 <-
      plotly::renderPlotly({
        datag <- myDatan()
        colNames <- names(datag)[-1]
        p <- plotly::plot_ly(data = datag,
                             x = ~Ano,
                             y = as.formula(paste0("~", colNames[1])),
                             type = "bar",
                             name = paste0(colNames[1])) %>%
          layout(yaxis = list(title = 'Valor'), barmode = 'stack',title="Valor dos Projetos por Ano e Fomentador")

        for(trace in colNames[-1]){
          p <- p %>% plotly::add_trace(y = as.formula(paste0("~", trace)), name = trace)
        }
        p
      })

############# GRÁFICO DA ABA NOVOS (NOVO)
    output$graph22 <-
      plotly::renderPlotly({
        datag2 <- myData2n()
        colNames <- names(datag2)[-1]
        p <- plotly::plot_ly(data = datag2,
                             x = ~Ano,
                             y = as.formula(paste0("~", colNames[1])),
                             type = "bar",
                             name = paste0(colNames[1])) %>%
          layout(yaxis = list(title = 'Número'), barmode = 'stack',title="Número de Projetos Novos por Ano e Fomentador")

        for(trace in colNames[-1]){
          p <- p %>% plotly::add_trace(y = as.formula(paste0("~", trace)), name = trace)
        }
        p
      })

############# GRÁFICO DA ABA CATEGORIAS (NOVO)
    output$graph52 <-
      plotly::renderPlotly({
        datag3 <- myData3n()
        colNames <- names(datag3)[-c(1:2)]
        p <- plotly::plot_ly(data = datag3,
                             x = ~Ano,
                             y = as.formula(paste0("~", colNames[1])),
                             type = "bar",
                             name = paste0(colNames[1])) %>%
          layout(yaxis = list(title = 'Número'), barmode = 'stack',title="Número de Categorias Nível 1 por Ano")

        for(trace in colNames[-1]){
          p <- p %>% plotly::add_trace(y = as.formula(paste0("~", trace)), name = trace)
        }
        p
      })

############# TABELA DE BUSCA (NOVO)
    output$tab12 <-
      DT::renderDT({
        datag0 <- myData_etl2()
        DT::datatable(datag0,
                  filter = 'top',
                  rownames = FALSE,
                  options = list(
                    scrollX = TRUE,
                    searching = TRUE,
                    pageLength = 10,
                    columnDefs = list(list(
                      targets = "_all",
                      render = DT::JS(
                        "function(data, type, row, meta) {",
                        "return type === 'display' && data != null && data.length > 15 ?",
                        "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                        "}"
                      )
                    ))
                  ))
      })


############# FILTRO FOMENTA
     observe({
        if(input$selectall == 0) return(NULL)
        else if (input$selectall%%2 == 0)
        {
          updateCheckboxGroupInput(session,"fomentos", "Fomentador:",
                                   choices = c("ANEEL", "ANP","BNDES","CNEN","CNPq","FAPESP","FINEP"))
        }
        else
        {
          updateCheckboxGroupInput(session,"fomentos", "Fomentador:",
                                   choices = c("ANEEL", "ANP","BNDES","CNEN","CNPq","FAPESP","FINEP"),
                                   selected = c("ANEEL", "ANP","BNDES","CNEN","CNPq","FAPESP","FINEP"))
        }
      })
  })}

