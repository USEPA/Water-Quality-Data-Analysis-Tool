
library(data.table)
library(dplyr)
library(ggplot2)
library(htmltools)
library(leaflet)
library(magrittr)
library(rCharts)
library(rkt)
library(scales)
library(shiny)
library(shinyBS)
library(shinydashboard)
#library(sparkline)
library(stringr)
library(grid)





# Sourcing functions files
source("external/Functions/functions.R")

# Sourcing module files
source("external/Modules/fileUpload.R")
source("external/Modules/button.R")

# File with the sidebar code
source("external/tabUI/sidebar.R", local = TRUE)

# The separate files composing the panels
homePanel <- source("external/tabUI/home.R", local = TRUE)$value
instructionsPanel <- source("external/tabUI/instructions.R", local = TRUE)$value
dataUploadPanel <- source("external/tabUI/dataUpload.R", local = TRUE)$value

#templatePanel <- source("external/tabUI/templates.R", local = TRUE)$value
stationsPanel <- source("external/tabUI/stationsTab.R", local = TRUE)$value
assessmentUnitsPanel <- source("external/tabUI/assessmentUnitsTab.R", local = TRUE)$value
criteriaPanel <- source("external/tabUI/CriteriaTab.R", local = TRUE)$value
analysisPanel <- source("external/tabUI/analysisTab.R", local = TRUE)$value
mapPanel <- source("external/tabUI/mapTab.R", local = TRUE)$value
chartPanel <- source("external/tabUI/chartTab.R", local = TRUE)$value
tablePanel <- source("external/tabUI/tableTab.R", local = TRUE)$value
assessUPanel <- source("external/tabUI/assessUTab.R", local = TRUE)$value
metalsPanel <- source("external/tabUI/metalsAnalysis.R", local = TRUE)$value

# Import the metals criteria file
metalsCriteria <- read.csv("external/Metals_Analysis_Example.csv")
























