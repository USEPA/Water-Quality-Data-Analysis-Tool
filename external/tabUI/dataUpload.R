
function(){
  tabItem(
    tabName = "data",
    br(),
    br(),
    h3("Upload data from the Data Discovery Tool"),
    br(),
    fluidRow(column(8, 
                    bsCollapse(open = c("Upload panel"),
                               multiple = TRUE,
                               bsCollapsePanel(title = "Upload panel", style = "info",
                                               csvFileInput("dataFile", "Upload the data file (.csv format)"))
                               
                               
                    )
    ),
    br(),
    br(),
    br(),
    br(),
    fluidRow(column(10, offset =1, 
                    uiOutput("meta"),
                    br(),
                    br(),
                    DT::dataTableOutput("discoveryDataTable")
    ))
    )
  )
}























