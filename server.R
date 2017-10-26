# server.R file
options(shiny.maxRequestSize=60*1024^2)

server <- function(input, output, session) {
  
  ############# Data Tab ##########################  
  dataFile <- callModule(csvFile, "dataFile",
                             stringsAsFactors = FALSE, skip = 10)
  
  WQP <- reactive({
    if(is.null(dataFile())){
      return(NULL)
    } else {
      dat <- dataFile()
      dat <- data.table(dat)
      dat[, Characteristic := str_to_title(as.character(Characteristic))] # Capitalizing all first letters of words since there was duplication caused by inconsistency in whether words were capitalized or not
      dat[, ActivityStartDate := as.Date(ActivityStartDate)] # '%Y-%m-%d')
      dat[, MonthDay := format(ActivityStartDate, format = "%m-%d")]
      dat$Result <- as.numeric(dat$Result)
      dat <- dat[!is.na(Result)]
      setnames(dat, c("ActivityMediaName", "ResultSampleFractionText"), c("Media", "Sample_Fraction"))
      datcols <- c("LatitudeMeasure", "LongitudeMeasure")
      dat[, (datcols) := lapply(.SD, as.numeric), .SDcols = datcols]

      return(dat)
      
    }})
  
  output$meta <- renderUI({
    data <- WQP()
    records <- nrow(data)
    char <- length(unique(data$Characteristic))
    station <- length(unique(data$Station))
    org <- length(unique(data$OrganizationFormalName))
    minDate <- min(data$ActivityStartDate)
    maxDate <- max(data$ActivityStartDate)
    
    fluidRow(p(h4(paste("The file contains data from", station, "stations in",
                        org, "organization(s) for the period", minDate, "to", maxDate,
                        ". The data include", char, "characteristics (parameters) in", records, "rows."))))
  })
  
  output$discoveryDataTable <- DT::renderDataTable(
    data.frame(WQP()),  escape = -1, rownames = FALSE,
    extensions = 'Buttons', options = list(dom = 'lfrBtip', buttons = I('colvis'), 
                                           pageLength = 100,
                                           lengthMenu = c(100, 200, 500),
                                           columnDefs = list(list(visible =  FALSE, targets = list(1,3,4,6,8,9,10,11,12,
                                                                                                   13, 14, 15, 16, 17, 18, 
                                                                                                   19, 20, 21, 22, 23, 24, 
                                                                                                   25, 26, 27, 28, 29, 30, 
                                                                                                   32, 35, 36, 37, 38, 39, 40,
                                                                                                   41, 42, 43, 44, 45, 46, 47,
                                                                                                   48, 49, 50, 51, 52, 53, 54, 
                                                                                                   55, 56, 57, 58, 59, 60, 61,
                                                                                                   62, 63, 64, 65, 66, 67, 68)))
    ), server = TRUE
  )
  

  ############# Criteria Tab ##########################  
  
  
  # Generate the empty criteria template
  criteria_gen <- reactive({
    
    Criteria <- data.table(WQP())
    Criteria <- Criteria[,.(Count = .N), by = c("Characteristic", "Unit", "Media", "Sample_Fraction") ]
    setnames(Criteria, c("Characteristic", "Unit"), c("PARM", "Units") )
    Criteria <- Criteria[, .(PARM, Units, Media, Sample_Fraction)]
    Criteria$USE_OR_CLASS <- ""
    Criteria$WBODY <- ""  
    Criteria$ECOREGION <- ""
    Criteria$Criterion <- ""  
    Criteria$Limit <- ""
    Criteria$Comparison <- ""
    Criteria$AverageTime <- ""
    Criteria$MinSamples <- ""
    Criteria$SeasonStartDate <- ""
    Criteria$SeasonEndDate <- ""
    
    return(Criteria)
    
  })

  output$Criteria_outfile <- downloadHandler(
    filename = function() {
      paste('Criteria-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.table(criteria_gen(), con, row.names = FALSE, quote = TRUE, sep = ",")
    })
  
  criteriaFile <- callModule(csvFile, "criteriaFile",
                         stringsAsFactors = FALSE)
  
  output$criteria_table <- DT::renderDataTable(
    criteriaFile(), escape = -1, rownames = FALSE
  )
  
  ############# Metals analysis Tab ##########################  
  
  output$Criteria_metals <- downloadHandler(
    filename = function() {
      paste('Metals-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.table(metalsCriteria, con, row.names = FALSE, quote = TRUE, sep = ",")
    })
  
  metalsFile <- callModule(csvFile, "metalscriteriaFile",
                             stringsAsFactors = FALSE)
  
  output$metals_table <- DT::renderDataTable(
    metalsFile(), escape = -1, rownames = FALSE
  )
  
  # Prepping the metals file and combining it with the criteria file
  metalsPrep <- reactive({
    metals <- data.table(metalsFile())
    metals[is.na(metals)] <- ""
    cols <- c("m", "b", "Hardness_default", "CF")
    metals[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    
    # Calculate conversion factors (CF) that are hardness-dependent 
    metals[is.na(CF) & !is.na(Hardness_default) & PARM == "Lead", CF := leadCF(Hardness_default)]
    metals[is.na(CF) & !is.na(Hardness_default) & PARM == "Cadmium"
           & grepl("acute", Criterion, ignore.case = TRUE), CF := cadCF_acute(Hardness_default)]
    metals[is.na(CF) & !is.na(Hardness_default) & PARM == "Cadmium"
           & grepl("chronic", Criterion, ignore.case = TRUE), CF := cadCF_chronic(Hardness_default)]
    
    # Calculating the limits (criteria) against which the result will be compared
    metals[grepl("acute", Criterion, ignore.case = TRUE), 
           Limit := round(limit_CMC(Hardness_default, m, b, CF), 2)]
    metals[grepl("chronic", Criterion, ignore.case = TRUE), 
           Limit := round(limit_CCC(Hardness_default, m, b, CF), 2)]
    
    # Removing columns no longer required
    metals[, c("m", "b", "Hardness_default", "CF") := NULL]
    
    # Adding a "Comparison" column to calculate exceedences later on
    metals[, Comparison := "GT"]
    metals[, ECOREGION := ""]
    
    # Renaming criteria names
    metals[Criterion == "Freshwater Acute", Criterion := "FW_Acute"]
    metals[Criterion == "Freshwater Chronic", Criterion := "FW_Chronic"]
    metals[Criterion == "Saltwater Acute", Criterion := "SW_Acute"]
    metals[Criterion == "Saltwater Chronic", Criterion := "sW_Chronic"]
    return(data.frame(metals))
    
  })
  
  criteria <- reactive({
     metals <- data.table(metalsPrep())
     criterion <- data.table(criteriaFile())
  
     # combining the metals and criteria files
     criterion <- rbind(criterion, metals, use.names = TRUE, fill = TRUE)
   
    return(criterion)
  })
  

############################# Stations tab ###############################
  # Generate the empty stations template
  
  station_gen<-reactive({
    if(is.null(WQP())){
      return(NULL)
    } else {
      dat <- data.table(WQP())
      stations <- dat[,.(Name = last(Name), Organization=last(Organization), OrganizationFormalName=last(OrganizationFormalName), LatitudeMeasure = last(LatitudeMeasure),
                         LongitudeMeasure = last(LongitudeMeasure)), by = Station ]
      
      stations$AssessmentUnit <- ""
      return(stations)
    }})
  
  
 
  output$Station_outfile <- downloadHandler(
    filename = function() {
      paste('Stations-', Sys.Date(), '.csv', sep='') 
    },
    content = function(con) {
      write.table(station_gen(), con, row.names = FALSE, sep = ",")
    })
  
  stationsFile <- callModule(csvFile, "stationsFile",
                             stringsAsFactors = FALSE)
  
  output$stations_table <- DT::renderDataTable(
    stationsFile(), escape = -1, rownames = FALSE
  )
  
  ############################# Assessment Units tab ###############################
  # Generate the empty assessment units template
  
  assessunit_gen<-reactive({
    if(is.null(stationsFile())){
      return(NULL)
    } else {
      dat <- data.table(stationsFile())
      AssesU <- dat[ , list(AssessmentUnit)]
      AssesU$USE_OR_CLASS <- ""  
      AssesU$WBODY <- "" 
      AssesU$ECOREGION <- "" 
      return(AssesU)
      
    }})
  
  
  output$AssessmentUnit_outfile <- downloadHandler(
    filename = function() {
      paste('Assessment_Units-', Sys.Date(), '.csv', sep='') 
    },
    content = function(con) {
      write.table(assessunit_gen(), con, row.names = FALSE, sep = ",")
    })
  

  assessmentUnitFile <- callModule(csvFile, "assessmentUnitFile",
                             stringsAsFactors = FALSE)
  
  output$assessmentUnit_table <- DT::renderDataTable(
    assessmentUnitFile(), escape = -1, rownames = FALSE
  )
  
 ######################## Analysis tab ###################################
 
   data_prep <- reactive({
     
    # Organize the inputs
    dat <- WQP()
    stations <- stationsFile()
    assesU <- assessmentUnitFile()
    criterion <- criteria()

    stations[is.na(stations)] <- ""
    assesU[is.na(assesU)] <- ""
    criterion[is.na(criterion)] <- ""
    
    stations <- data.table(stations)
    assesU <- data.table(assesU)
    criterion <- data.table(criterion)
    
    criterion[, nc := nchar(SeasonStartDate)]
    criterion[, SeasonStartDate := as.character(SeasonStartDate)]
    criterion[nc < 4, SeasonStartDate := paste0("0", SeasonStartDate)]
    criterion[, SeasonStartDate := paste0("2000", SeasonStartDate)]
    criterion[, SeasonStartDate := as.Date(SeasonStartDate, format = "%Y%m%d")]
    criterion[, SeasonStartDate := format(SeasonStartDate, format = "%m-%d")]
   
    criterion[, nc := nchar(SeasonEndDate)]
    criterion[, SeasonEndDate := as.character(SeasonEndDate)]
    criterion[nc < 4, SeasonEndDate := paste0("0", SeasonEndDate)]
    criterion[, SeasonEndDate := paste0("2000", SeasonEndDate)]
    criterion[, SeasonEndDate := as.Date(SeasonEndDate, format = "%Y%m%d")]
    criterion[, SeasonEndDate := format(SeasonEndDate, format = "%m-%d")]
    criterion[, nc := NULL]
    
    cols <- c("WBODY", "ECOREGION", "Sample_Fraction",  "Media", "Criterion", "Comparison", "Units")
    criterion[, (cols) := lapply(.SD, as.character), .SDcols = cols]
    
    # To be used with "Filtered data"
    cols2 <- c("Station", "Characteristic", "Unit", "Media","Sample_Fraction", "Name", "OrganizationFormalName")
    dat[, (cols2) := lapply(.SD, as.character), .SDcols = cols2]
  
    assesU[, AssessmentUnit := as.character(AssessmentUnit)]
    stations[, AssessmentUnit := as.character(AssessmentUnit)]
    
    step1 <- merge(stations, assesU, by = "AssessmentUnit", allow.cartesian = TRUE)
    
    step2A <- merge(step1, criterion, by=c("USE_OR_CLASS", "WBODY", "ECOREGION"), allow.cartesian = TRUE)
    setnames(step2A, "PARM", "Characteristic")
    setnames(dat, "Unit", "Units")
    step3A <- merge(step2A, dat, by = c("Station", "Characteristic", "Units", "Media","Sample_Fraction",
                                        "Name", "Organization", "OrganizationFormalName",
                                        "LatitudeMeasure", "LongitudeMeasure"), all = TRUE,
                    allow.cartesian = TRUE)
    step3A <- step3A[!is.na(Result)]
    cols3 <- c("Station", "Characteristic", "Units", "Sample_Fraction",
               "OrganizationFormalName", "WBODY", "Media", "ECOREGION", 
               "Criterion", "USE_OR_CLASS")
    step3A[, (cols3) := lapply(.SD, as.character), .SDcols = cols3]
    
    return(step3A)
   })
  
  rolling <- reactive({
    data <- data_prep()
    data <- data[grepl("rolling", Criterion, ignore.case = TRUE)]
    
    data[, maxDate := max(ActivityStartDate, na.rm = TRUE), by = c("Station", "Characteristic",
                                                                   "AssessmentUnit", "Units",
                                                                   "USE_OR_CLASS", "WBODY", "Media","Sample_Fraction",
                                                                   "Sample_Fraction", "ECOREGION"  )]
    data[, MinSamples := as.numeric(MinSamples)]
    data[is.na(MinSamples), MinSamples := 0]
    
    data[, id := paste(Station, Characteristic,
                       AssessmentUnit, Units,
                       USE_OR_CLASS, WBODY, Media,
                       Sample_Fraction, ECOREGION, sep = "_")]
    data[, count := .N, by ="id"]
    data <- data[count >= MinSamples]
    
    ids <- unique(data$id)
    all_ids <- lapply(ids, function(y){
      sub_date <- data[id == y]
      dates <- as.Date(unique(sub_date$ActivityStartDate))
      if(!is.na(dates)){
        sapply(dates, function(x) {
          endDate <- x + as.numeric(unique(sub_date$AverageTime))
          if(is.null(endDate)){
            paste("EndDate doesn't exist")
          } else {
            if(endDate > unique(sub_date$maxDate)) {
              #  return()
              res = "End date is after the latest date for this parameter/station combination"
            } else {
              subsub <- sub_date[ActivityStartDate >= as.Date(x) & ActivityStartDate <= endDate]
              res <- mean(subsub$Result, na.rm = TRUE)
            }
            sub_date[ActivityStartDate == as.Date(x), RollingAvg := res]
          }
        })
        return(sub_date)
      } else {
        return()
      }
    }) 
    
    rollingAverages <- rbindlist(all_ids, use.names = TRUE)
    rollingAverages[, c("id", "count") := NULL]
    rollingAverages <- rollingAverages[, list(Station, Characteristic, ActivityStartDate, 
                                              AssessmentUnit, Units, USE_OR_CLASS, WBODY, 
                                              Media, Sample_Fraction, ECOREGION, RollingAvg)]
    setkey(rollingAverages)
    rollingAverages <- rollingAverages[!duplicated(rollingAverages)]
    
    return(rollingAverages)
    
  })
    
   ANALYSIS <- eventReactive(input$RunAnalysis, {
     
    test <- data_prep()
    rollingAverages <- rolling()
    test <- merge(test, rollingAverages, by = c("Station", "Characteristic", "ActivityStartDate",
                                                   "AssessmentUnit", "Units" , "USE_OR_CLASS", "Media",
                                                   "WBODY","Sample_Fraction","ECOREGION" ), all = TRUE)
    
    # Identifying exceedences
    test[, Exceed := FALSE]
    test[Comparison == "GT" & Result > Limit, Exceed := TRUE]
    test[Comparison == "LT" & Result < Limit, Exceed := TRUE]
    test[, Geomean_AU := geomean(Result), by = c("AssessmentUnit", "Characteristic", 
                                                 "Units", "USE_OR_CLASS", "WBODY", "Media",
                                                "Sample_Fraction",  "ECOREGION")]
    test[, Geomean_Station := geomean(Result), by = c("Station", "Characteristic", 
                                                      "Units", "USE_OR_CLASS", "WBODY", "Media",
                                                     "Sample_Fraction", "ECOREGION")]
    
    test[Criterion == "Geomean_Station" & Geomean_Station > Limit, 
         Exceed := TRUE]
    test[Criterion == "Geomean_AU" & Geomean_AU > Limit, 
         Exceed := TRUE]
    
    test[, `:=` (SeasonStartDate = as.character(SeasonStartDate),
                 SeasonEndDate = as.character(SeasonEndDate))]
    test[grepl("season", Criterion, ignore.case = TRUE) & !is.na(SeasonEndDate) & !is.na(SeasonStartDate) & Result  > Limit &
           MonthDay > SeasonStartDate & MonthDay < SeasonEndDate,
         Exceed := TRUE]
    
    test[grepl("Roll", Criterion, ignore.case = TRUE), Exceed := FALSE]
    test[grepl("Roll", Criterion, ignore.case = TRUE) & 
           !is.na(RollingAvg) & 
           RollingAvg > Limit, Exceed := TRUE]
    
    return(test)
   })
  
    table_dat<-reactive({
      table_dat<-ANALYSIS()
      table_dat[is.na(table_dat)]<-0
      cols <- c("Station", "Characteristic", "USE_OR_CLASS", "WBODY", "Sample_Fraction",
                "ECOREGION", "Criterion", "Name", "Organization")
      table_dat[, (cols) := lapply(.SD, as.character), .SDcols = cols]
      
      table_dat[, `:=` (num_exceed_total = sum(Exceed),
                        count_total = .N),  by = c("Station", 
                                                   "Characteristic", 
                                                   "USE_OR_CLASS",
                                                   "WBODY",
                                                   "ECOREGION",
                                                   "Media",
                                                   "Sample_Fraction",
                                                   "Criterion", 
                                                   "Comparison")]
      table_dat<-table_dat[, .(AssessmentUnit = last(AssessmentUnit),
                               Name = last(Name), 
                               num_exceed = sum(Exceed), 
                               count = .N, 
                               num_exceed_total = last(num_exceed_total),
                               count_total = last(count_total),
                               Lat = last(LatitudeMeasure),
                               Long = last(LongitudeMeasure)), by = c("Station", 
                                                                      "Characteristic", 
                                                                      "USE_OR_CLASS",
                                                                      "WBODY",
                                                                      "ECOREGION",
                                                                      "Media",
                                                                      "Sample_Fraction",
                                                                      "Criterion", 
                                                                      "Comparison")]
      
      
      table_dat[, Perc_Exceed := round((num_exceed_total/count_total) * 100, digits = 2)]
      table_dat[, Perc_Exceed_Map := ifelse(Perc_Exceed == 0, 1, Perc_Exceed)]
      table_dat[Criterion == 0, Criterion := "Not Analyzed"]
      table_dat[USE_OR_CLASS == 0, USE_OR_CLASS := "Not Analyzed"]
      table_dat[AssessmentUnit == 0, AssessmentUnit := "Not Specified"]
      table_dat[Sample_Fraction == 0, Sample_Fraction := "Not Specified"]
      
      # Moving the assessment unit column to the first position
      setcolorder(table_dat, c("AssessmentUnit", setdiff(names(table_dat), "AssessmentUnit")))
      return(table_dat)
      
    })
    
    output$Save_Analysis <- downloadHandler(
      filename = function() {
        paste('Analysis-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.table(ANALYSIS(), con, row.names = F, col.names = TRUE, sep = ",")
      })
    
    output$analysis_table = 
      DT::renderDataTable(table_dat(),
                          rownames = FALSE,
                          filter = 'top',
                          extensions = 'Buttons',  escape = -1, options = list(dom = 'lfrBtip', buttons = I('colvis'), 
                                                                 columnDefs = list(list(visible =  FALSE, targets = list(4, 5, 6, 7, 9, 10, 13, 14, 15, 17))
                                                                                   ))
                          
                         
      )

    ################## Attains-compatible output ############################################
    attains <- reactive({
      stations <- data.table(stationsFile())
      assesU <- data.table(assessmentUnitFile())
      stations[is.na(stations)] <- ""
      assesU[is.na(assesU)] <- ""
      assesU[, AssessmentUnit := as.character(AssessmentUnit)]
      stations[, AssessmentUnit := as.character(AssessmentUnit)]
      table_dat <- table_dat()
        
      stationNames <- stations[, list(Station, AssessmentUnit)]
      stations2 <- merge(stationNames, assesU, by = "AssessmentUnit", allow.cartesian = TRUE)
      au <- merge(stations2, table_dat, by = c("Station", "USE_OR_CLASS", "WBODY","ECOREGION", "AssessmentUnit") )
      au[, station_count := length(unique(Station)), by = "AssessmentUnit"]
      au2 <- au[, .(num_exceed_total = sum(num_exceed_total, na.rm = TRUE),
                    count_total = sum(count_total, na.rm = TRUE)
      ),
      by = c("AssessmentUnit", "USE_OR_CLASS", "Characteristic", "station_count")]
      au2[, Perc_Exceed := round((num_exceed_total/count_total) * 100, 1)]
      au2[, USE_ATTAINMENT_CODE := ""]
      setnames(au2, c("AssessmentUnit", "USE_OR_CLASS", "Characteristic"),
               c("ASSESSMENT_UNIT_ID", "USE_NAME", "PARAM_NAME"))
      setcolorder(au2, c("ASSESSMENT_UNIT_ID", "USE_NAME", "USE_ATTAINMENT_CODE",
                         "PARAM_NAME","num_exceed_total", "count_total",        
                         "Perc_Exceed", "station_count"))
      
      return(au2)
    })
    
    output$attains_table = 
      DT::renderDataTable(attains(),
                          rownames = FALSE,
                          filter = 'top')
    
    
    # Downloading the ATTAINS analysis table
    output$Save_ATTAINS_table <- downloadHandler(
      filename = function() {
        paste('ATTAINS-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.table(attains(), con, row.names = F, col.names = TRUE, sep = ",")
      })
                          
    ########### Trends analysis ############################################
    output$trend_selects <- renderUI({
      fluidRow(column(6,
                      selectizeInput("time_choice", h4("Select time unit of analysis"),
                                     choices = c("Month", "Year")),
                      selectizeInput("tie", h4("If multiple observations exist for same date, use:"),
                                     choices = c("Mean" = "a", 
                                                 "Median" = "m")),
                      checkboxInput("correct", h4("Correct for correlation between blocks (should only be chosen if there are more than nine years of data)"),
                                    value = FALSE)
                      
      ),
      column(6,
             selectizeInput("block_choice", h4("Select unit of analysis"),
                            choices = c("Stations", "Assessment Units")),
             numericInput("p_choice", h4("Select the p-value"),
                          value = 0.05)
             
             )
      )
    })
    
     trends_prep <- reactive({
       data <- copy(WQP()) #[Characteristic == input$parm_trend]
       if(is.element("Unit", names(data))){
         setnames(data, "Unit", "Units")
        }
       data <- merge(data, stationsFile(), by = "Station")
       data_trend <- merge(data, assessmentUnitFile(), 
                           "AssessmentUnit", allow.cartesian = TRUE)
       
       data <- data_trend[, list(Station, Characteristic, Units, Result, ActivityStartDate, AssessmentUnit)]
       data <- data[!duplicated(data)]
       data <- gen_date(data)
       data[, date_char := as.character(ActivityStartDate)]
       data[, year := as.numeric(tstrsplit(date_char, "-")[[1]])]
       data[, month := as.numeric(tstrsplit(date_char, "-")[[2]])]
       data[, month_dec := month * (1/12) ]
       data[, date := year + month_dec]
       data[, stationID := as.numeric(factor(Station))]
       data[, assessUID := as.numeric(factor(AssessmentUnit))]
       return(data)
     })


     trends_result <- eventReactive(input$RunTrendAnalysis, {
       dataT <- trends_prep()
       dataT[, charunit := paste(Characteristic, Units, sep = "_")]
       
       if(input$block_choice == "Stations"){
         if(input$time_choice == "Year"){
           dataT[, count := length(unique(year)), by = c("Station", "Characteristic", "Units")]
         } else {
           dataT[, count := length(unique(date)), by = c("Station", "Characteristic", "Units")]
         }

         data_less <- dataT[count < 4] 
         data_less <- data_less[, list(Station, Characteristic, Units)]
         data_less <- data_less[!duplicated(data_less)]
         if(input$time_choice == "Year"){
           data_less[, Kendall_score := "Less than four years in the sample"]
         } else {
           data_less[, Kendall_score := "Less than four months in the sample"]
         }
         dataT <- dataT[count >= 4]
         if(nrow(dataT) > 0) {
           stations <- unique(dataT$Station)
           
           all_results <- lapply(stations, function(x){
             data_s <- dataT[Station == x]
             chars <- unique(data_s$charunit)
             
             station_result <- lapply(chars, function(y){
                data <- data_s[charunit == y]
               if(input$time_choice == "Year"){
                 timeUnit <- data$year
               } else {
                 timeUnit <- data$date
               }
               trend <- rkt(timeUnit, data$Result, rep = "a")
               character <- unique(data[charunit == y, Characteristic])
               unit <- unique(data[charunit == y, Units])
               result <- data.table(Station = x,
                                    Characteristic = character,
                                    Units = unit,
                                    Kendall_score = round(trend[[2]], 4),
                                    Theil_Sen_slope = round(trend[[3]], 4),
                                    Kendall_tau = round(trend[[12]], 4),
                                    p_value = round(trend[[1]], 4),
                                    variance = round(trend[[4]], 4)
               )
               return(result)
               
             })
             
             sub_result <- rbindlist(station_result, use.names = TRUE)
             
           })
           
           
           all <- rbindlist(all_results, use.names = TRUE)
           all[, Trend := trend_sig(Theil_Sen_slope, p_value, input$p_choice), by = c("Station", "Characteristic", "Units")]
           
         } else {
           if(input$time_choice == "Year"){
             all <- data.table(Station = "No station/characteristic pair has at least four sampling years")
           } else {
             all <- data.table(Station = "No station/characteristic pair has at least four sampling months")
           }
         }
         all <- rbind(all, data_less, use.names = TRUE, fill = TRUE)
         
       } else if(input$block_choice == "Assessment Units") {
         if(input$time_choice == "Year"){
           dataT[, count := length(unique(year)), by = c("AssessmentUnit", "Characteristic", "Units")]
         } else {
           dataT[, count := length(unique(date)), by = c("AssessmentUnit", "Characteristic", "Units")]
         }

         data_less <- dataT[count < 4] 
         data_less <- data_less[, list(AssessmentUnit, Characteristic, Units)]
         data_less <- data_less[!duplicated(data_less)]
         if(input$time_choice == "Year"){
           data_less[, Kendall_score := "Less than four years in the sample"]
         } else {
           data_less[, Kendall_score := "Less than four months in the sample"]
         }         
         
         dataT <- dataT[count >= 4]
        
          if(nrow(dataT) > 0) {
           
         AU <- unique(dataT$AssessmentUnit)
         
         all_results <- lapply(AU, function(x){
           data_s <- dataT[AssessmentUnit == x]
           chars <- unique(data_s$charunit)
           
           au_result <- lapply(chars, function(y){
             data <- data_s[charunit == y]
             if(input$time_choice == "Year"){
               timeUnit <- data$year
             } else {
               timeUnit <- data$date
             }
             trend <- rkt(timeUnit, data$Result, data$stationID, rep = input$tie)
             character <- unique(data[charunit == y, Characteristic])
             unit <- unique(data[charunit == y, Units])
             result <- data.table(AssessmentUnit = x,
                                  Characteristic = y,
                                  Units = unit,
                                  Kendall_score = round(trend[[2]], 4),
                                  Theil_Sen_slope = round(trend[[3]], 4),
                                  Kendall_tau = round(trend[[12]], 4),
                                  p_value = round(trend[[1]], 4),
                                  variance = round(trend[[4]], 4)
             )
             return(result)
             
           })
           
           sub_result <- rbindlist(au_result, use.names = TRUE)
           
         })
         
         
         all <- rbindlist(all_results, use.names = TRUE)
         all[, Trend := trend_sig(Theil_Sen_slope, p_value, input$p_choice), by = c("AssessmentUnit", "Characteristic")]
         } else {
           if(input$time_choice == "Year"){
             all <- data.table(AssessmentUnit = "No assessment unit/characteristic pair has at least four sampling years")
           } else {
             all <- data.table(AssessmentUnit = "No assessment unit/characteristic pair has at least four sampling months")
           }
         }
         all <- rbind(all, data_less, use.names = TRUE, fill = TRUE)
       }
        return(all)
     })
     
     output$trends_table <- DT::renderDataTable(
       trends_result(), escape = -1, rownames = FALSE,
       filter = 'top'
     )
     
     # Downloading the criteria analysis table
     output$Save_Analysis_trends <- downloadHandler(
       filename = function() {
         paste('Trends_Analysis-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.table(trends_result(), con, row.names = F, col.names = TRUE, sep = ",")
       })
     
     output$tbl_text <- renderText({
       str(trends_prep())
     })
     
  ################################ Map tab ##########################################

    output$ParamMAP <- renderUI({
      data <- ANALYSIS()
      data[, CharUnit := paste(Characteristic, " (", Units, ")", sep = "")]
      
      selectizeInput("param_map", label = p("Select a parameter"),
                     choices = unique(data[, CharUnit]), multiple = TRUE,
                     selected = if(input$param_sel==1){
                       unique(data[, CharUnit])
                     } else {NULL})
    })
    
    output$Class_Use_MAP <- renderUI({
      data <- ANALYSIS()
      selectizeInput("useclass_map", label = p("Select a use or class"),
                     choices = unique(data[!is.na(USE_OR_CLASS), USE_OR_CLASS]), multiple = TRUE,
                     selected = if(input$useclass_sel==1){
                       unique(data[!is.na(USE_OR_CLASS), USE_OR_CLASS])
                     } else {NULL})
    })
    
    output$Eco_MAP <- renderUI({
      data <- ANALYSIS()
      selectizeInput("eco_map", label = p("Select an ecoregion"),
                     choices = unique(data[, ECOREGION]), multiple = TRUE,
                     selected = if(input$eco_sel==1){
                       unique(data[, ECOREGION])
                     } else {NULL})
    })
    
    output$MediaMAP <- renderUI({
      data <- ANALYSIS()
      selectizeInput("media_map", label = p("Select media "),
                     choices = unique(data[, Media]), multiple = TRUE,
                     selected = if(input$media_sel == 1){
                       unique(data[, Media])
                     } else {NULL})
    })
    
    output$SampleMAP <- renderUI({
      data <- ANALYSIS()
      selectizeInput("sample_map", label = p("Select a sample fraction"),
                     choices = unique(data[, Sample_Fraction]), multiple = TRUE,
                     selected = if(input$sample_sel==1){
                       unique(data[, Sample_Fraction])
                     } else {NULL})
    })
    
    output$WB_MAP <- renderUI({
      data <- ANALYSIS()
      selectizeInput("wb_map", label = p("Select a water body"),
                     choices = unique(data[, WBODY]), multiple = TRUE,
                     selected = if(input$wb_sel==1){
                       unique(data[, WBODY])
                     } else {NULL})
    })

    spfilter_dat <- eventReactive (input$submit_filters, {
      data <- ANALYSIS()
      data[, CharUnit := paste(Characteristic, " (", Units, ")", sep = "")]
      if(!is.null(input$param_map) & !unique(data$CharUnit) %in% c("", NA)) {
        data <- data[CharUnit %in% input$param_map]
      }
      if(!is.null(input$useclass_map) & !unique(data$USE_OR_CLASS) %in% c("", NA)){
        data <- data[USE_OR_CLASS %in% input$useclass_map]
      }
      if(!is.null(input$eco_map) & !unique(data$ECOREGION) %in% c("", NA)){
        data <- data[ECOREGION %in% input$eco_map]
      }
      if(!is.null(input$media_map) & !unique(data$Media) %in% c("", NA)){
        data <- data[Media %in% input$media_map]
      }
      if(!is.null(input$sample_map) & !unique(data$Sample_Fraction) %in% c("", NA)){
        data <- data[Sample_Fraction %in% input$sample_map]
      }
      if(!is.null(input$wb_map) & !unique(data$WBODY) %in% c("", NA)){
        data <- data[WBODY %in% input$wb_map]
      }
      return(data)
    })
    
    displayed_data <- reactive({
      if(input$submit_filters == 0){
        data <- ANALYSIS()
      } else {
        data <- spfilter_dat()
      }
      
      return(data)
    })
    
    # Adding text if the analysis hasn't been run and the map isn't displayed
    output$non_display <- renderUI({
      h3("")
      if(input$RunAnalysis){
        h3("")
      } else {
        h3("Please run the criteria analysis in the Analysis tab by clicking the 'Run Analysis' button ", 
           style  = "text-align:center ; color: #990000 ;")
      }
    }) 
    
    # Adding text under map only if the map will be drawn
    output$map_text <- renderUI({
      if(input$RunAnalysis){
        tags$ul(tags$li("The station markers are scaled based on the # of exceedances/# of measurements."),
                tags$li("Stations marked with black circles have no exceedance records in the dataset."))
      }
    })
    
    min_date <- reactive({
      data <- displayed_data()
      min_date <- min(unique(data$ActivityStartDate), na.rm = TRUE)
    })
    
    max_date <- reactive({
      data <- displayed_data()
      max_date <- max(unique(data$ActivityStartDate), na.rm = TRUE)
    })
    
    map_df <- reactive({
      data <- displayed_data()
      data[, `:=` (num_exceed_total = sum(Exceed, na.rm = TRUE),
                        count_total = .N),  by = c("Station")]
      data <- data[, .(Name = last(Name), 
                               num_exceed = sum(Exceed), 
                               count = .N, 
                               num_exceed_total = last(num_exceed_total),
                               count_total = last(count_total),
                               Lat = last(as.numeric(as.character(LatitudeMeasure))),
                               Long = last(as.numeric(as.character(LongitudeMeasure))),
                       AssessmentUnit = last(AssessmentUnit)
                       ), by = c("Station")]
      
      
      data[, Perc_Exceed := (num_exceed_total/count_total) * 100]
      data[, Perc_Exceed_Map := ifelse(Perc_Exceed == 0, 1, Perc_Exceed)]
      
      return(data)
    })
   
     output$map_data = 
      DT::renderDataTable(spfilter_dat(),
                          rownames = FALSE,
                          filter = 'top')
    
   output$map<-renderLeaflet({
      radiusFactor <- 50
      
      leaflet(map_df()) %>%
        fitBounds(lng1 = ~min(Long), lat1 = ~min(Lat), lng2 = ~max(Long), lat2 = ~max(Lat)) %>%
        addTiles( "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png") %>%
        clearMarkers() %>% 
        addCircleMarkers(
           lat =~Lat, 
           lng = ~Long, 
           radius = ~(log(Perc_Exceed_Map) + 2)  * radiusFactor / 5^2,
           layerId = row.names(map_df()),
           clusterOptions = markerClusterOptions()
          
        )
    })
    
    # Render polygons
    observe({
      pal <- colorFactor("Set1", domain = map_df()$AssessmentUnit)
      radiusFactor <- 50
      map <- leafletProxy("map", data = map_df())
      map %>% clearMarkers()
      if(input$mapcolor == "Assessment_Units") {
        map %>% clearMarkerClusters() %>% 
          addCircleMarkers(data = map_df(), 
                                 color = ~pal(AssessmentUnit),
                                 lat =~Lat, 
                                 lng = ~Long, 
                                 radius = ~(log(Perc_Exceed_Map) + 2)  * radiusFactor / 5^2,
                                 layerId = row.names(map_df()),
                                 clusterOptions = markerClusterOptions()
                               )
      } else if(input$mapcolor == "Exceedances") {
        map %>% clearMarkerClusters() %>% 
          addCircleMarkers(data = map_df(), 
                                 color =  ~ifelse(num_exceed_total == 0, 'black','blue'),
                                 lat =~Lat, 
                                 lng = ~Long, 
                                 radius = ~(log(Perc_Exceed_Map) + 2)  * radiusFactor / 5^2,
                                 layerId = row.names(map_df()),
                                 clusterOptions = markerClusterOptions()
        )
        
      }

    })
    
    observeEvent(input$map_marker_click, {
      leafletProxy("map") %>% clearPopups()
      content<- as.character(tagList(
        tags$html(
          tags$div(style = 'color: #24476B', 
                   tags$ul(
                     tags$li(h4(paste("Station ID: ", map_df()[row.names(map_df()) == input$map_marker_click["id"], ][["Station"]]))),
                     tags$li(h4(paste("Station name: ", map_df()[row.names(map_df()) == input$map_marker_click["id"], ][["Name"]]))),
                     tags$li(h4(paste("Start date: ", min_date()))),
                     tags$li(h4(paste("End date: ", max_date())))
                                     
                             )
          )
        )
        
        ))
      
      leafletProxy("map") %>% addPopups(lat = input$map_marker_click$lat, lng = input$map_marker_click$lng, 
                                        paste(content, '<br></br>',
                                              actionButton("Stat_Summary", "Select this Location", 
                                                           onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'),
                                              sep = ""))
    })
    
#################################  Station Summary  ##########################################
    
    # *** Specifying station to be summarized
    station_info <- eventReactive(input$button_click,{
      map_df()[row.names(map_df()) == input$map_marker_click$id]
    })
    
    station_data1 <- eventReactive(input$button_click, {
      data <- ANALYSIS()[Station == station_info()$Station ]
      data[, CharUnit := paste(Characteristic, " (", Units, ")", sep = "")]
      return(data)
    })
    
    station_data <- reactive({
      data <- station_data1()
      data <- data[USE_OR_CLASS == input$Select_Use ]
      return(data)
    })
    
    output$Station_Summary_Panel <- renderUI({
      h4(paste("Summary for station ", station_info()$Name))
    })
    
    output$Station_Summary_select<-renderUI({
      data <- station_data1()
      if(is.null(data)){
        h3("Please select a station on the map in the Map tab")
        
      } else {
        use_list<-as.character(unique(data[!is.na(USE_OR_CLASS), USE_OR_CLASS]))
        fluidRow(column(4), column(4, selectizeInput("Select_Use", "Select a Use or Class", choices = use_list, selected = use_list[1])))    
      }
      
    })
    
    station_summary <- reactive({
      data <- station_data()
      stat_summary <- data[, list(Characteristic, ActivityStartDate, Criterion, Exceed)]
      stat_summary <- stat_summary[!duplicated(stat_summary)]
      stat_summary <- stat_summary[, `:=` (Exceedances = sum(Exceed, na.rm = TRUE),
                                           Measurements = .N), by = c("Characteristic")]
      stat_summary <- stat_summary[!duplicated(stat_summary[, list(Characteristic, Exceedances, Measurements)])]
      stat_summary[, Exceed := NULL]
      
    })
    
    output$Station_Summary_text<-renderUI({
      data <- station_data()
      use_list <- as.character(unique(data$USE_OR_CLASS))
      name <- station_info()$Name
      stat_summary <- station_summary()
      
      measurements <- dim(data)[1]
      chars <- length(unique(WQP()[Station == station_info()$Station]$Characteristic))
      criterion <- length(stat_summary$Characteristic)
      date_low <- min(data$ActivityStartDate)
      date_high <- max(data$ActivityStartDate)
      fluidRow(
        column(7, 
               p(tagList(
                 tags$html(
                   tags$div(style = 'text-align:left', 
                            tags$ul(
                              tags$li(h5(paste("Station name: ", name))),
                              tags$li(h5(paste("Start date: ", date_low))),
                              tags$li(h5(paste("End date: ", date_high))),
                              tags$li(h5(paste("Class/use: ", input$Select_Use))),
                              tags$li(h5(paste("Unique characteristics: ", chars))),
                              tags$li(h5(paste("Measurements: ", measurements))),
                              tags$li(h5(paste("Number of criteria: ", criterion)))
                            )
                   )
                 )
                 
               ))
        )
               )
      
    })
    
    # Adding text when no station is selected
    output$chart_text <- renderUI({
        h4("If no charts are rendered on this page, please choose a station on the map")
    })
    
    frequency_plot <- function(){
      data <- station_data()
      p1 <- ggplot(data, aes(x = ActivityStartDate, y = Characteristic)) +
        geom_point(color = ifelse(data$Exceed == TRUE, "red", "black"), 
                   size = 5, alpha = 1/2)+
        labs(x = '',y='') +
        theme_bw()+
        scale_y_discrete(labels = function(y) str_wrap(y, width = 20)) +
        scale_x_date(labels = date_format("%b-%d-%y"))+
        theme(axis.text.x=element_text(angle=35, vjust=1, hjust=1),
              legend.position = "bottom")
      print(p1)
    }
    
    output$Station_data_time_plot<-renderPlot({
      frequency_plot()
    })
    
    # Download the frequency chart
    output$freq_chart <- downloadHandler(
      filename = function() { 
        paste0("Frequency_chart-", Sys.Date(), '.png') 
        },
      content = function(file) {
        png(file)
        frequency_plot()
        dev.off()
      },
      contentType = "image/png")
    
    
    output$barplot <- renderChart2({
 
      check <- station_summary()
      
      a <- rCharts:::Highcharts$new()
      a$chart(type = "bar")
      a$title(text = "Measurements vs. Exceedances")
      if(length(unique(check$Characteristic)) == 1){
        a$xAxis(categories = list(check$Characteristic), labels = list(reserveSpace = 'false'))
      } else {
        a$xAxis(categories = check$Characteristic, labels = list(reserveSpace = 'false'))
        }
      a$data(check[, .(Exceedances, Measurements)])
      a$exporting(enabled = TRUE)
      return(a)
    })
    
    output$Station_time <- renderUI({
      data <- charunit()
      data <- data[!duplicated(data[, list(Date, CharUnit)])]  
      selectizeInput("CHAR_UNITS", label = p("Please Choose Criteria"),
                     choices = unique(data[, CharUnit]), multiple = FALSE)
    })
    
    isolate({
      charunit <- reactive({
        data<-data.table(station_data())
        data[, CharUnit := paste(Characteristic, " (", Units, ")", sep = "")]
        setnames(data, "ActivityStartDateTime", "Date")
        data <- data[, list(Result, CharUnit, Date, Characteristic)]
        data[, charlength := length(Date), by = 'CharUnit']
        data <- data[charlength > 0]
        return(data)
      })
      
      timedata <- reactive({
        data <- charunit()
        data[, Result := as.numeric(Result)]
        data <- data[CharUnit == input$CHAR_UNITS]
        data <- data[!duplicated(data[, list(Date, CharUnit)])]  
        testc <- dcast(data, Date ~ CharUnit, value.var = 'Result')
        return(testc)
      })
    })
    
    criteriaValues <- reactive({
      data <- station_data()[CharUnit == input$CHAR_UNITS]
      data <- data[, list(Criterion, Limit)]
      data <- data[!duplicated(data)]
      criteria <- data$Criterion
      lim <- data$Limit
      return(list(criteria = criteria, lim = lim))
    })
    
    output$timeseries <- renderChart2({
      datatime <- data.table(timedata())
     # datatime[, Date := gsub(" UTC", "", Date)]
      datatime[, Date :=  as.numeric(as.POSIXct(datatime$Date, format = "%m/%d/%Y", tz = "" ))*1000]
      
      datatime <- datatime[order(Date)]
      values <- criteriaValues()

      ln <- rCharts::Highcharts$new()
      ln$colors("#08519C","#D94801" )
      ln$xAxis(type = 'datetime', labels = list(format = '{value:%m/%d/%Y}'))
      if(!is.null(values)){
        ln$yAxis( plotLines = list(
          list(value = values[[2]][1], color= 'red', dashStyle= 'shortdash', width=2, 
               label = list(text = values[[1]][1], align = 'left')),
          list(value = values[[2]][2], color= 'red', dashStyle= 'shortdash', width=2, 
               label = list(text = values[[1]][2], align = 'left')),
          list(value = values[[2]][3], color= 'red', dashStyle= 'shortdash', width=2, 
               label = list(text = values[[1]][3], align = 'left')),
          list(value = values[[2]][4], color= 'red', dashStyle= 'shortdash', width=2, 
               label = list(text = values[[1]][4], align = 'left')),
          list(value = values[[2]][5], color= 'red', dashStyle= 'shortdash', width=2, 
               label = list(text = values[[1]][5], align = 'left'))
    )
    )
     }
      for(i in 2:ncol(datatime)) {
        ln$series(
          data = toJSONArray2(datatime[, c(1,i), with = FALSE], names = FALSE, json = FALSE),
          name = names(datatime)[i],
          type = 'spline',
          yAxis = (i-2)
        )
      }
      
      ln$plotOptions(spline = list(connectNulls = TRUE))
      ln$chart(marginTop = 70, zoomType = 'xy', panKey = 'shift', panning = TRUE) 
      ln$exporting(filename = "Line chart")
      
      return(ln)
    })
    #################################  Assessment Unit Summary  ##########################################
    # *** Specifying assessment unit to be summarized
    output$AssesU_Summary_select<-renderUI({
      data <- assessmentUnitFile()
      assess_list <- as.character(unique(data$AssessmentUnit))
      fluidRow(column(4), column(4, selectizeInput("Select_AssessU", "Select an Assessment Unit", choices = assess_list, selected = assess_list[1])))    
    })
    
    assess_summary<-reactive({
      data<-data.table(ANALYSIS())
      data<-data[AssessmentUnit == input$Select_AssessU]
      return(data)
    })
    
    output$AssessU_text<-renderUI({
      x<-length(unique(assess_summary()$Station))
      fluidRow(p(paste("You are viewing a summary of the assessment unit ", input$Select_AssessU, " .",
                       "This assessment unit contains ", x, " station(s).")))
    })
    
    
    isolate({
      output$Assess_Use_select<-renderUI({
        data <- assess_summary()
        assess_use_list <- as.character(unique(data$USE_OR_CLASS))
        fluidRow(column(4), column(4, selectizeInput("Select_Assess_Use", "Select a Use/Class from this Assessment Unit", choices = assess_use_list, selected = assess_use_list[1])))    
      })
      
   
      assess_stat_summary<-reactive({
        asData <- assess_summary()
        asData[, Exceedances := sum(Exceed, na.rm = TRUE), by = c("Station", 
                                                                  "Criterion", 
                                                                  "Characteristic", 
                                                                  "Units",
                                                                  "USE_OR_CLASS", 
                                                                  "WBODY", 
                                                                  "Media",
                                                                  "Sample_Fraction",
                                                                  "ECOREGION",
                                                                  "Sample_Fraction")]
        asData[, Measurements := .N, by = c("Station", 
                                            "Criterion", 
                                            "Characteristic", 
                                            "Units",
                                            "USE_OR_CLASS", 
                                            "WBODY", 
                                            "Media", 
                                            "Sample_Fraction",
                                            "ECOREGION",
                                            "Sample_Fraction")]
        asData <- asData[, list(Station, Characteristic,USE_OR_CLASS, WBODY, Media, Sample_Fraction, ECOREGION, Sample_Fraction, Criterion,
                                Measurements, Exceedances)]
        asData <- asData[!duplicated(asData)]
        
        
        return(asData)
      })
      
      output$assess_Stat_table = 
        DT::renderDataTable(assess_stat_summary(),
                            rownames = FALSE)
    })
    
    output$Assess_time <- renderUI({
      data <- charunit_assess()
      data <- data[!duplicated(data[, list(Date, CharUnit)])]  
      selectizeInput("CHAR_UNITS_A", label = p("Please Choose Criteria"),
                     choices = unique(data[, CharUnit]), selected = unique(data[, CharUnit])[1], multiple = FALSE)
    })
    
    isolate({
      charunit_assess <- reactive({
        data<-data.table(assess_summary()[USE_OR_CLASS == input$Select_Assess_Use])
        data[, CharUnit := paste(Characteristic, " (", Units, ")", sep = "")]
        setnames(data, "ActivityStartDateTime", "Date")
        data <- data[, list(Result, CharUnit, Date, Characteristic, Station)]
        data[, charlength := length(Date), by = 'CharUnit']
        data <- data[charlength > 0]
        return(data)
      })
      
      timedata_assess <- reactive({
        data <- charunit_assess()
        data[, Result := as.numeric(Result)]
        data <- data[CharUnit == input$CHAR_UNITS_A]
        data <- data[!duplicated(data[, list(Date, Station)])]  
        testc <- dcast(data, Date ~ Station, value.var = 'Result')
        return(testc)
      })
    })
    
    criteriaValues2 <- reactive({
      data <- assess_summary()[USE_OR_CLASS == input$Select_Assess_Use]
      data[, CharUnit := paste(Characteristic, " (", Units, ")", sep = "")]
      data <- data[CharUnit == input$CHAR_UNITS_A]
      
      data <- data[, list(Criterion, Limit)]
      data <- data[!duplicated(data)]
      criteria <- data$Criterion
      lim <- data$Limit
      return(list(criteria = criteria, lim = lim))
    })
    
    
    output$timeseries_assess <- renderChart2({
      datatime <- data.table(timedata_assess())
      datatime[, Date := gsub(" UTC", "", Date)]
      datatime$Date =  as.numeric(as.POSIXct(datatime$Date, "%m/%d/%Y", tz = "" ))*1000
      datatime <- datatime[order(Date)]
      values <- criteriaValues2()
      
      ln <- rCharts:::Highcharts$new()
      ln$xAxis(type = 'datetime', labels = list(format = '{value:%m/%d/%Y}'))
      for(i in 2:ncol(datatime)) {
        ln$series(
          data = toJSONArray2(datatime[, c(1,i), with = FALSE], names = FALSE, json = FALSE),
          name = names(datatime)[i],
          type = 'spline'
        )}
      
      if(!is.null(values)){
        ln$yAxis( plotLines = list(
          list(value = values[[2]][1], color= 'red', dashStyle= 'shortdash', width=2, 
               label = list(text = values[[1]][1], align = 'left')),
          list(value = values[[2]][2], color= 'red', dashStyle= 'shortdash', width=2, 
               label = list(text = values[[1]][2], align = 'left')),
          list(value = values[[2]][3], color= 'red', dashStyle= 'shortdash', width=2, 
               label = list(text = values[[1]][3], align = 'left')),
          list(value = values[[2]][4], color= 'red', dashStyle= 'shortdash', width=2, 
               label = list(text = values[[1]][4], align = 'left')),
          list(value = values[[2]][5], color= 'red', dashStyle= 'shortdash', width=2, 
               label = list(text = values[[1]][5], align = 'left'))
        )
        )
      }
      
      ln$plotOptions(spline = list(connectNulls = TRUE, marker = list(enabled = TRUE)))
      ln$chart(marginTop = 70, zoomType = 'xy', panKey = 'shift', panning = TRUE) 
      ln$exporting(filename = "Line chart")
      
      return(ln)
    }) 

  }
  
  

























