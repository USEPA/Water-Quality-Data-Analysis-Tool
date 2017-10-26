
function(){
  tabItem(
    tabName = "map",
    br(),
    br(),
    uiOutput("non_display"),
    fluidRow(column(4, offset = 8,
                    selectizeInput('mapcolor', h4("Select the criterion for coloring map markers:"),
                                   choices = c("Exceedances",
                                               "Assessment_Units"))
                    )),
    leafletOutput("map"),
    br(),
    uiOutput("map_text"),
    # p("The station markers are scaled based on the # of exceedances/# of measurements."),
    # p("Stations marked with black circles have no exceedance records in the dataset."),
    br(),
    DT::dataTableOutput("map_data"),
    bsCollapsePanel(h3("Apply Filters to Map",style  = "text-align:center"), style = "info",
                    fluidRow(column(5),
                             column(2, bsButton("submit_filters", "Submit!")),
                             bsPopover("submit_filters", "Click Submit after applying filters", "Only filters with items selected will be applied. Note: At least one station must be selected.",
                                       "top", trigger = "hover", options = list(container = "body"))),
                    fluidRow(column(6,
                                    h4(radioButtons('useclass_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))),
                             column(6, 
                                    h4(radioButtons('media_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))
                                    )),
                    fluidRow(column(6, 
                                    h4(uiOutput("Class_Use_MAP"), style  = "text-align:center")),
                             column(6,  
                                    h4(uiOutput("MediaMAP"), style  = "text-align:center"))),
                    fluidRow(column(6,
                                    h4(radioButtons('eco_sel', "", c("Select All"=1, "Deselect All"=2), selected =1)
                                       )),
                             column(6, 
                                    h4(radioButtons('wb_sel', "", c("Select All"=1, "Deselect All"=2), selected =1)))),
                    fluidRow(column(6,
                                    h4(uiOutput("Eco_MAP"), style  = "text-align:center")),
                             column(6,
                                    h4(uiOutput("WB_MAP"), style  = "text-align:center")
                                    )),
                    fluidRow(column(6, 
                                    h4(radioButtons('sample_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))),
                             column(6,
                                    h4(radioButtons('param_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))
                                    )),
                    fluidRow(column(6,
                                    h4(uiOutput("SampleMAP"), style  = "text-align:center")
                                    ),
                             column(6,  
                                    h4(uiOutput("ParamMAP"), style  = "text-align:center"))),
    br()
  )
  )
}























