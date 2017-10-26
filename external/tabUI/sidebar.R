
sm <- sidebarMenu(
  br(),
  menuItem(tabName = "home",
           text = "Home",
           icon = icon("home")),
  menuItem(tabName = "instructions",
           text = "Instructions",
           icon = icon("list")),
  menuItem(tabName = "data",
           text = "Data Upload",
           icon = icon("upload")),
  menuItem(tabName = "stations",
           text = "Stations",
           icon = icon("institution")),
  menuItem(tabName = "assessmentUnits",
           text = "Assessment Units",
           icon = icon("object-group")),
  menuItem(tabName = "criteria",
           text = "Criteria",
           icon = icon("table")),
  menuItem(tabName = "metals",
           text = "Metals Analysis",
           icon = icon( "flask")),
  menuItem(tabName = "analysis",
           text = "Analysis",
           icon = icon("line-chart")),
  menuItem(tabName = "map",
              text = "Map",
              icon = icon( "map")),
  menuItem(tabName = "summary",
           text = "Station summary",
           icon = icon("book"),
           menuSubItem(tabName = "chart",
                    text = "Charts",
                    icon = icon("bar-chart"))),
           # menuSubItem(tabName = "table",
           #             text = "Table",
           #             icon = icon("table")),
  menuItem(tabName = "assessUSummary",
              text = "Assessment Unit Summary",
              icon = icon("file"))

  
)































