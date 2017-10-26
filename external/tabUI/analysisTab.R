
function(){
  tabItem(
    tabName = "analysis",
    h3(""),
    bsCollapse(open = "Run analysis for criteria",
      bsCollapsePanel(title = "Run analysis for criteria", style = "info",
                      fluidRow(#column(6,
                                #      numericInput("duration", h4("Number of days for calculating rolling averages"),
                                 #                  value = 30)
                                  #    ),
                      column(6, offset = 5,
                                      actionButton("RunAnalysis", h4('Run Analysis'))
                      )),
                      br()#,
                      
                    #  DT::dataTableOutput("metalsTest")
                      ),
      bsCollapsePanel(title = "Run trends analysis", style = "info",
                        uiOutput("trend_selects") ,
                      fluidRow(column(4, offset = 5,
                                      actionButton("RunTrendAnalysis", h4('Run Trends Analysis')),
                                      br()
                                      
                                      
                      ))),
                      bsCollapsePanel(title = "Results of criteria analysis", style = "info",
                                      fluidRow(column(5), 
                                               column(2, downloadButton("Save_Analysis", "Save Analysis Data"))),
                                      bsPopover("Save_Analysis", "Save Data", "Click to download a .csv file containing the complete analysis data set.",
                                                "top", trigger = "hover", options = list(container = "body")),
                                      br(),
                                      fluidRow(
                                        column(10, offset = 0.5,
                                               
                                                 DT::dataTableOutput("analysis_table")
                                        )
                                        
                                      )
                                      ),
                      bsCollapsePanel(title = "ATTAINS-compatible analysis summary", style = "info",
                                      fluidRow(column(5),
                                        column(2, 
                                               downloadButton('Save_ATTAINS_table', h5('Save Table'), icon = icon("download")),
                                               br(),
                                               br())),
                                        fluidRow(DT::dataTableOutput("attains_table")
                                        )
                        
                                      
                      ),
                      bsCollapsePanel(title = "Results of trends analysis", style = "info",
                                      h4("Please note that with large files this may take a few moments."),
                                      br(),
                                      fluidRow(column(5),
                                               column(2, 
                                                      downloadButton('Save_Analysis_trends', h5('Save Table'), icon = icon("download")),
                                                      br(),
                                                      br())),
                                      DT::dataTableOutput("trends_table")
                                  # textOutput("tbl_text")
                      )
                      
                      
    )
   
    
  )
}



















