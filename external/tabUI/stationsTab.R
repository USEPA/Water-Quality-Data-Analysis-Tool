
function(){
  tabItem(
    tabName = "stations",
    br(),
    fluidRow(column(8, #offset =1, 
                    bsCollapse(open = c("Download panel"),
                               multiple = TRUE,
      bsCollapsePanel(title = "Download panel", style = "info",
                      downloadButton("Station_outfile", "Download the stations template", icon = icon("download"))
      )
      )
      )
      ),
      br(),
    fluidRow(column(8, 
                    bsCollapse(open = c("Upload panel"),
                               multiple = TRUE,
                               bsCollapsePanel(title = "Upload panel", style = "info",
                                               csvFileInput("stationsFile", "Upload the stations file (.csv format)"))
                               
                    
                    )
                    ),
             br(),
             br(),
             fluidRow(column(10, offset =1, 
                             DT::dataTableOutput("stations_table")
             ))
             )
      )
}
































