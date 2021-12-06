#' edicao UI Function
#' @import DataEditR
#' @import shinyjs
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_edicao_ui <- function(id){
  ns <- NS(id)
  tagList(useShinyjs(),
          dataInputUI(ns("input1")),
          dataSelectUI(ns("select1")),
          dataFilterUI(ns("filter1")),
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
                                  col_stretch = TRUE
    )

    observe({
      values$data_active <- data_update()
    })

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
