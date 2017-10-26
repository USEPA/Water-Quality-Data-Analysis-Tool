

# Module UI function
displayTableUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  DT::dataTableOutput(ns("table"))
  
}

displayTable <- function(input, output, session, data){
  output$tbl = DT::renderDataTable(
    data(), row.names = FALSE)
  
}

# Module for download handler
downloadFileUI <- function(id, label) {
  ns <- NS(id)
  downloadButton(ns("download"), label, icon = icon("download"))
}


downloadFile <- function(input, output, session, data) {
  
  {
  
























