% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csvFileInput.R
\name{csvFileInput}
\alias{csvFileInput}
\title{csvFileInput: Module UI function}
\usage{
csvFileInput(id, label = "CSV file")
}
\arguments{
\item{id}{The namespace for the module}

\item{label}{Label, Default: 'CSV file'}
}
\value{
UI
}
\description{
Shiny module UI for csv file input
}
\details{
DETAILS
}
\examples{
library(shiny)
# Only run examples in interactive R sessions

if(interactive()){
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        csvFileInput("datafile", "User data (.csv format)")
      ),
      mainPanel(
        DT::dataTableOutput("table")
      )
    )
  )
  server <- function(input, output, session) {
    datafile <- callModule(csvFile, "datafile",
                           stringsAsFactors = FALSE)
    
    output$table <- DT::renderDataTable({
      datafile()
    })
  }
  
  shinyApp(ui, server)
    
}
}
