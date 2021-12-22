#' edicao UI Function
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import RSQLite
#' @import DataEditR
#' @importFrom shiny NS tagList
mod_edicao_ui <- function(id){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
          dataInputUI(ns("input1")),
          dataSelectUI(ns("select1")),
          dataFilterUI(ns("filter1")),
          actionButton(ns("trava"),label = "", icon = icon("lock", lib = "font-awesome")),
          dataSyncUI(ns("sync1")),
          dataOutputUI(ns("output-1")),
          dataEditUI(ns("edit1"))
  )
}

#' edicao Server Functions
#'
#' @noRd
mod_edicao_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    nome_colunas <- c("X","id","fonte_dados",
                        "data_assinatura","data_limite","duracao_dias",
                        "titulo_projeto", "status_projeto", "valor_contratado",
                        "valor_executado_2013_2020", "nome_agente_financiador",
                        "natureza_agente_financiador", "modalidade_financiamento",
                        "nome_agente_executor",    "natureza_agente_executor",
                        "uf_ag_executor", "regiao_ag_executor",
                        "p.d_ou_demonstracao",     "valor_executado_2013",
                        "valor_executado_2014",    "valor_executado_2015",
                        "valor_executado_2016",    "valor_executado_2017",
                        "valor_executado_2018",    "valor_executado_2019",
                        "valor_executado_2020",    "motor",
                        "categorias")
    values <- reactiveValues(data = NULL, data_active = NULL,
                             rows = NULL, columns = NULL)

    data_input <- dataInputServer("input1")

    observeEvent(data_input(), {
      values$rows <- NULL
      values$columns <- NULL
      values$data <- data_input()
    })

    data_select <- dataSelectServer("select1", data = reactive(values$data),
                                    hover_text = "selecione as colunas")
    data_filter <- dataFilterServer("filter1", data = reactive(values$data),
                                    hover_text = "filtre linhas")
    observe({
      values$rows <- data_filter$rows()
      values$columns <- data_select$columns()
    })

    observe({
      if (length(values$rows) == 0 & length(values$columns) ==
          0) {
        values$data_active <- values$data
      }
      else {
        if (length(values$rows) != 0 & length(values$columns) ==
            0) {
          values$data_active <- values$data[values$rows,
                                            , drop = FALSE]
        }
        else if (length(values$rows) == 0 & length(values$columns) !=
                 0) {
          values$data_active <- values$data[, values$columns,
                                            drop = FALSE]
        }
        else if (length(values$rows) != 0 & length(values$columns) !=
                 0) {
          values$data_active <- values$data[values$rows,
                                            values$columns, drop = FALSE]
        }
      }
    })



      data_update <- dataEditServer("edit1", data = reactive(values$data_active),
                                    col_names = FALSE,
                                    col_edit = FALSE,
                                    row_edit = FALSE,
                                    col_stretch = TRUE)



    observe({
      values$data_active <- data_update()
    })


#observeEvent(input$trava,{
#  data_update <- dataEditServer("edit1", data = reactive(values$data_active),
#                                col_names = FALSE,
#                                col_edit = FALSE,
#                                row_edit = FALSE,
#                                col_stretch = TRUE)
#                          }
#)


    data_sync <- dataSyncServer("sync1", data = reactive(values$data),
                                data_subset = reactive(values$data_active),
                                rows = reactive(values$rows),
                                columns = reactive(values$cols),
                                hover_text = "synchronise")
    observe({values$data <- data_sync() })


    dataOutputServer("output-1",
                     data = reactive({ values$data_active})
    )


    #teste
    observeEvent(input$cut, {
      if (values$cut) {
        values$cut <- FALSE
        updateButton(session, "cut", NULL, block = FALSE,
                     style = "danger")
      }
      else {
        values$cut <- TRUE
        updateButton(session, "cut", NULL, block = FALSE,
                     style = "success")
      }
    })
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
    observeEvent(input$done, {
      if (hide == TRUE) {
        stopApp(values$data_active)
      }
      else {
        if (values$cut) {
          stopApp(values$data_active)
        }
        else {
          stopApp(values$data)
        }
      }
    })


  })
}

## To be copied in the UI
# mod_edicao_ui("edicao_1")

## To be copied in the server
# mod_edicao_server("edicao_1")
