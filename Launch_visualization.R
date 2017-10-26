ifelse(as.numeric(R.Version()$major)<3,
       {writeLines("You are running an old version of R. \nPlease Update to R Version 3.2 or higher. \nThe Quick Start Guide provides step by step instructions to update R.")},{
       #ifelse(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor)<2.1,
              #{writeLines("You are running an old version of R. \nPlease Update to R Version 3.2.1 or higher. \nThe Quick Start Guide provides step by step instructions to update R.")}, {
              # Install packages if needed
            # if ("dataRetrieval" %in% rownames(installed.packages()) ){
            #   packinfo<-installed.packages(fields = c("Package", "Version"))
            #   if (as.numeric(substr(packinfo["dataRetrieval", "Version"], 1, 3))<2.3){
            #     remove.packages("dataRetrieval")
            #     install.packages("dataRetrieval", repos='http://cran.cnr.Berkeley.edu')
            #   }  
            # }
              packageNeeds <- c('shiny', 'shinyBS', 'data.table', 'DT',
                  'dplyr', 'dataRetrieval', 'devtools', 'httr', 'grid',
                  'ggplot2', 'stringr', 'scales', 'rkt', 'magrittr', 'htmltools', 'shinydashboard')
              packageNeeds <- packageNeeds[!packageNeeds %in% rownames(installed.packages())]
              if(length(packageNeeds)>0){
                install.packages(packageNeeds, repos='http://cran.cnr.Berkeley.edu')
              }
              devPackages<-c("leaflet", "rCharts")
              devPackages <- devPackages[!devPackages %in% rownames(installed.packages())]
              if (length(devPackages)>0){
                require(devtools)
                if ("rCharts" %in% devPackages){
                  devtools::install_github('ramnathv/rCharts')
                }
                if("leaflet" %in% devPackages){
                  devtools::install_github("rstudio/leaflet")
                }
              }
            ## run the app
            require(shiny)
            runApp(launch.browser = T)
       })

