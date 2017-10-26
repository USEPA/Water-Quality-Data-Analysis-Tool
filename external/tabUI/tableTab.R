
function(){
  tabItem(
    tabName = "table",
    br(),
    br(),
    fluidRow(column(5), column(2, downloadButton("Save_Analysis", "Save Analysis Data"))),
    bsPopover("Save_Analysis", "Save Data", "Click to download a .csv file containing the complete analysis data set.",
              "top", trigger = "hover", options = list(container = "body")),
    br(),
    fluidRow(
      column(10, offset = 0.5,
             DT::dataTableOutput("analysis_table")
      )
      
    )
  )
}























