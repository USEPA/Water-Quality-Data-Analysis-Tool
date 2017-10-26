
function(){
  tabItem(
    tabName = "chart",
    uiOutput("chart_text"),
   # br(),
  #  br(),
    h4(uiOutput("Station_Summary_Panel"), style  = "text-align:center"),
    h5(uiOutput("Station_Summary_select"), style  = "text-align:center"),
    h5(uiOutput("Station_Summary_text"), style  = "text-align:center"),
    br(),
  #  showOutput("station_scatter", "highcharts"),
    showOutput("barplot", "highcharts"),
    h4("Sampling Frequency", style  = "text-align:center"),
    plotOutput("Station_data_time_plot"),
    h5("* Red points indicate exceedances"),
    fluidRow(column(3, offset = 5,
                  downloadButton('freq_chart', h5('Save Image'), icon = icon("download"))
                  )),
  br(),
    wellPanel(h4("Time Series Chart", style  = "text-align:center"),
              uiOutput('Station_time', style  = "text-align:center"),
              showOutput('timeseries', 'highcharts')
           )
  )
}























