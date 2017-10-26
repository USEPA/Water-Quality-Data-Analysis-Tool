
function(){
  tabItem(
    tabName = "instructions",
    fluidRow(column(1),
             column(9,
                    tags$html(
                      tags$div(style = 'color: #24476B', 
                               h2('Instructions'),
                               tags$ul(
                                 
tags$li(h4('The STORET Data Analysis Tool works with the output from the STORET Data Discovery Tool to provide users with advanced analytical options such as comparison to established criteria, geomeans and trends.  To run the tool, you first need to teach it about your dataset.  This is accomplished by uploading several comma separated files (.csv) that are described below.  Templates for the necessary data elements are provided for you.  The data must be provided in the template formats for the tool to work.;'),
                                 
tags$li(h4('Filtered Data – This is the file output by the Data Discovery Tool that contains all the monitoring data downloaded from the Water Quality Portal.'), style = 'text-align: justify;'),
                                 
tags$li(h4('Stations File – This file contains information on the Monitoring Stations that the data was obtained from.  Using this file you can associate assessment units with the monitoring stations.  When you download the Stations File Template the following data elements are pulled from the filtered data file and pre-populated for you: station identifier, name, owning organization, organization formal name, latitude/longitude coordinate.  The assessment unit field is populated by the user.  '), style = 'text-align: justify;'),
                                 
tags$li(h4('Assessment Units – Using this file you can associate beneficial uses and/or class information; waterbody type and ecoregion information with the assessment units.  When you download this template, the Assessment unit information is pulled from the station file that you previously upload and populated for you.  You fill in the Use and/or class, waterbody type and ecoregion.  Note, if waterbody type and/or ecoregion are not applicable to your situation, you do not need to populate these fields.'), style = 'text-align: justify;'),
                                 
tags$li(h4('Criterion – this file is used to tell the tool which criteria to use and which statistical analysis to run.  When you download this template, the parameter name, associated use, waterbody type, ecoregion are pulled from the files that you previously upload and populated for you.  You fill in the criterion, limit, Comparison type, Sample Fraction and units.  '), style = 'text-align: justify;'),
                                
tags$li(h4('Once you’ve uploaded all the necessary files, go to the View Data Tab and click on Run Analysis.'), style = 'text-align: justify;'),

tags$li(h4(''), style = 'text-align: justify;')
                                 ),
                               br(),
                               h3('Notes:'),
                               tags$ul(
                                 tags$li(h4('In the criteria file, if a rolling average is included, the column "AverageTime" has to be populated by the number of days over which rolling averages are calculated. '), style = 'text-align: justify;'),
                                 tags$li(h4(''), style = 'text-align: justify;'),
                                 tags$li(h4(''), style = 'text-align: justify;')
                                 )
                                 )
                                 )
                    
                                 )
                                 )
                      )
)
  
    
  
}

































