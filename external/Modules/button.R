
# Module UI function
downloadbuttonUI <- function(id, label = "Download the file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
 #   fileInput(ns("file"), label)
    downloadButton(ns("download"), label, icon = icon("download"))
    )
  
    
}

# Module server function
downloadbutton <- function(input, output, session, labels, data) {
 
   output$download <- downloadHandler(
    filename = function() {
      labels
    },
    content = function(con) {
      write.table(data(), con, row.names = FALSE, sep = ",")
    })
}



















