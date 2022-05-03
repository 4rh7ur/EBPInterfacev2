#' mod_valid UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mod_valid_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      #shiny::column(
      shiny::fileInput(ns("dataset1"), "Indique o Diretório do Dataset a Ser Enriquecido",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                       ),
      shiny::fileInput(ns("dataset2"), "Indique o Diretório do Dataset com as informações a serem inseridas",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
      shiny::downloadButton(ns("download"), "Executar Validação e Baixar Dataset"),
      #width = 10)#column
    )#fluidrow

  )
}

#' mod_valid Server Functions
#'
#' @noRd
mod_mod_valid_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # myData_enriq <- reactive({
    #   inFile <- input$dataset1
    #   if (is.null(inFile)) return(NULL)
    #   data_enriq <- fread(inFile$datapath, header = input$header, sep = input$sep, nrows = as.numeric(input$nrows))
    #
    #   return(data)
    # })

    # data_enriq<- reactive({
    #   data_enriq<- read.csv(input$dataset1$datapath)
    # })

    #Etapa que faz a inserção dos dados modificados na base de dados intermediária

    myData_inser <- reactive({

      #puxando o dataset original

      inFile <- input$dataset1
      if (is.null(inFile)) return(NULL)
      data_enriq <- readr::read_csv(inFile$datapath, locale = readr::locale(encoding = "latin1"))
      data_enriq <- data_enriq %>% dplyr::mutate(id = as.character(id))

      #dataset com alterações

      inFile2 <- input$dataset2
      if (is.null(inFile2)) return(NULL)
      data_inser <- readr::read_csv(inFile2$datapath, locale = readr::locale(encoding = "latin1"))
      data_inser <- data_inser %>% dplyr::mutate(id = as.character(id))

      data <- dplyr::left_join(data_enriq, data_inser,
                               by = c("id" = "id"),
                               suffix = c("_original", "_inserido")) %>%
                      unique()

      data <- data %>%
        dplyr::select(-contains("_original"))%>%
        dplyr::rename_at(dplyr::vars(dplyr::contains("_inserido")), list(~stringr::str_remove(., "_inserido")))

      return(data)
    })

    #Fazer o Download
    output$download <- downloadHandler(
      filename = function() {
        paste(myData_inser(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(myData_inser(), file, row.names = FALSE)
      }
    )

    # data_inser <- reactive({
    #   data_inser <- read.csv(input$dataset2$datapath)
    # })

  })
}

## To be copied in the UI
# mod_mod_valid_ui("mod_valid_1")

## To be copied in the server
# mod_mod_valid_server("mod_valid_1")
