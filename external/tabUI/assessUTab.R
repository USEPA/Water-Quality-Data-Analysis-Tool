
function(){
  tabItem(
    tabName = "assessUSummary",
    br(),
    br(),
    h4(uiOutput('AssesU_Summary_select', style  = "text-align:center")),
    h4(uiOutput('AssessU_text', style  = "text-align:center")),
    wellPanel(h4("Assessment Unit Time Series Chart", style  = "text-align:center"),
              h4(uiOutput('Assess_Use_select', style  = "text-align:center")),
              uiOutput('Assess_time', style  = "text-align:center"),
              showOutput('timeseries_assess', 'highcharts')
              ),
    br(),
    fluidRow(
      column(10, offset = 0.5,
             DT::dataTableOutput("assess_Stat_table")
      ))
  )
}























