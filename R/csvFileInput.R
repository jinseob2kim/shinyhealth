#' @title csvFileInput: Module UI function
#' @description Shiny module UI for csv file input
#' @param id The namespace for the module
#' @param label Label, Default: 'CSV file'
#' @return UI
#' @details DETAILS
#' @examples 
#' library(shiny)
#' # Only run examples in interactive R sessions
#'
#' if(interactive()){
#'   
#'   ui <- fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(
#'         csvFileInput("datafile", "User data (.csv format)")
#'       ),
#'       mainPanel(
#'         DT::dataTableOutput("table")
#'       )
#'     )
#'   )

#'   server <- function(input, output, session) {
#'     datafile <- callModule(csvFile, "datafile",
#'                            stringsAsFactors = FALSE)
#'     
#'     output$table <- DT::renderDataTable({
#'       datafile()
#'     })
#'   }
#'   
#'   shinyApp(ui, server)
#'     
#' }
#' @rdname csvFileInput
#' @export 
#' @importFrom shiny NS tagList fileInput checkboxInput selectInput 



csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading",value = T),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}


#' @title csvFile: Module server function
#' @description Shiny module for csv file input
#' @param input The namespace for the module
#' @param output datatable
#' @param session session
#' @param stringsAsFactors Convert all character columns to factors?
#' @return data.table (data.table package)
#' @details DETAILS
#' @examples 
#' library(shiny)
#' # Only run examples in interactive R sessions
#'
#' if(interactive()){
#'   
#'   ui <- fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(
#'         csvFileInput("datafile", "User data (.csv format)")
#'       ),
#'       mainPanel(
#'         DT::dataTableOutput("table")
#'       )
#'     )
#'   )

#'   server <- function(input, output, session) {
#'     datafile <- callModule(csvFile, "datafile",
#'                            stringsAsFactors = FALSE)
#'     
#'     output$table <- DT::renderDataTable({
#'       datafile()
#'     })
#'   }
#'   
#'   shinyApp(ui, server)
#'     
#' }
#' @rdname csvFile
#' @export 
#' @importFrom data.table fread 
#' @importFrom shiny reactive validate need observe

csvFile <- function(input, output, session, stringsAsFactors = F) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    fread(userFile()$datapath,
          header = input$heading,
          quote = input$quote,
          stringsAsFactors = stringsAsFactors)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}
