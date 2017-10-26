
WQP <- reactive({
  if(is.null(dataFile())){
    return(NULL)
  } else {
    dat <- dataFile()
    dat <- data.table(dat)
    #  dat[, c("X", "X.1", "X.2") := NULL]
    dat[, ActivityStartDate := as.Date(ActivityStartDate, '%m/%d/%Y')] # '%Y-%m-%d')
    dat <- data.table(dat)
    dat$Result <- as.numeric(dat$Result)
    dat <- dat[!is.na(Result)]
    return(dat)
    
  }})















