
function(){
  tabItem(
    tabName = "assessmentUnits",
    br(),
    fluidRow(column(8, #offset =1, 
                    bsCollapse(open = c("Download panel"),
                               multiple = TRUE,
      bsCollapsePanel(title = "Download panel", style = "info",
                      downloadButton("AssessmentUnit_outfile", "Download the assessment units template", icon = icon("download")))
      )
      )
      ),
      br(),
    fluidRow(column(8, 
                    bsCollapse(open = c("Upload panel"),
                               multiple = TRUE,
                               bsCollapsePanel(title = "Upload panel", style = "info",
                                               csvFileInput("assessmentUnitFile", "Upload the assessment units file (.csv format)"))
                               
                    
                    )
                    ),
             br(),
             br(),
             fluidRow(column(10, offset =1, 
                             DT::dataTableOutput("assessmentUnit_table")
             ))
             )
      )
}
































