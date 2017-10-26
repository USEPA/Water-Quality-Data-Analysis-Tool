
# Header -----------------------------------------------------------

header <- dashboardHeader(title = "", titleWidth = 250) 

# Sidebar --------------------------------------------------------------

sidebar <- dashboardSidebar(width = 250, sm)

# Dashboard body --------------------------------------------------

body <- dashboardBody(
  tags$head(tags$style(HTML('
                            .content-wrapper,
                            .right-side {
                            background-color: #ffffff;
                            }
                            ')
                       )
            ),
  tabItems(
    homePanel(),
    instructionsPanel(),
    dataUploadPanel(),
    stationsPanel(),
    assessmentUnitsPanel(),
    criteriaPanel(),
    metalsPanel(),
    analysisPanel(),
    mapPanel(),
    chartPanel(),
  #   tablePanel()
    assessUPanel()
  )
)

# Setup Shiny app UI components -------------------------------------------

ui <- dashboardPage(header, sidebar, body, skin = "blue")





















