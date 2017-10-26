
function(){
  tabItem(
    tabName = "criteria",
    br(),
    fluidRow(column(8, #offset =1, 
                    bsCollapse(open = c("Download panel"),
                               multiple = TRUE,
      bsCollapsePanel(title = "Download panel", style = "info",
                      downloadButton("Criteria_outfile", "Download the criteria template", icon = icon("download")))
                    #downloadbuttonUI("criteria", label = "Download the criteria file")
      
      )
      )),
      br(),
    fluidRow(column(8, 
                    bsCollapse(open = c("Upload panel"),
                               multiple = TRUE,
                               bsCollapsePanel(title = "Upload panel", style = "info",
                                               csvFileInput("criteriaFile", "Upload the criteria file (.csv format)"))
                               
                    
                    )
                    ),
             br(),
             br(),
             fluidRow(column(10, offset =1, 
                             DT::dataTableOutput("criteria_table")
             ))
             )
      )
}
































