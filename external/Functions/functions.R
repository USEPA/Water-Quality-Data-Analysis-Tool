

# Function to calculate a geometeric mean
geomean <- function(y) {
  n <- length(y)
  ansgeo <- prod(y, na.rm = TRUE)^(1/n)
  ansgeo
}

# Function to assign a verdict as to the significance and sign of the slope of trend analysis
trend_sig <- function(slope, p_est, p) {
  if(!is.null(p_est) & !is.na(p_est)){
    if(p_est <= p) {
      signif <- "Significant"
    } else {
      signif <- "Insignificant"
    }
    
    if(slope > 0) {
      sign <- "Positive"
    } else if (slope < 0) {
      sign <- "Negative"
    } else {
      sign <- "Zero slope"
    }
    
    verdict <- paste(sign, signif, sep = ", ")
    
  } else {
    verdict <- ""
  }
  
  return(verdict)
}

# Function to generate the needed variables for running the trends analysis
trends_data <- function(data) {
  data <- data[, list(Station, Result, ActivityStartDate, AssessmentUnit)]
  data <- data[!duplicated(data)]
  data[, date_char := as.character(ActivityStartDate)]
  data[, year := as.numeric(tstrsplit(date_char, "-")[[1]])]
  data[, month := as.numeric(tstrsplit(date_char, "-")[[2]])]
  data[, month_dec := month * (1/12) ]
  data[, date := year + month_dec]
  data[, stationID := as.numeric(factor(Station))]
  data[, assessUID := as.numeric(factor(AssessmentUnit))]
  
  return(data)
  
}

# Generate year, month and day columns for the activity start date
gen_date <- function(data){
  data[, date_char := as.character(ActivityStartDate)]
  data[, year := as.numeric(tstrsplit(date_char, "-")[[1]])]
  data[, month := as.numeric(tstrsplit(date_char, "-")[[2]])]
  data[, day := as.numeric(tstrsplit(date_char, "-")[[3]])]
  data[, monthday := as.numeric(paste0(month, day))]
  return(data)
}

# Calculate the rolling averages
roll <- function(data, start, end) {
  subData <- data[ActivityStartDate >= start & ActivityStartDate <= end]
  subData[, .(value = mean(Result, na.rm = TRUE))]
  return(unique(subData$value))
}

# Converting number to month-day vector of class date
toDate <- function(v) {
  if(nchar(v) < 4){
    v <- as.character(paste0("0", v))
  } else {
    v <- as.character(v)
  }
  
  d <- paste0("2000", v)
  d <- as.Date(d, format = "%Y%m%d")
  dd <- format(d, format = "%m-%d")
  return(dd)
}

na_to_name <- function(x, name) {
  if(is.na(x) | x == "") x <- name
}

# Function to compute hardness-dependant metals' criteria 

limit_CMC <- function(h, mA, bA, cf) {
  limit <- (exp(mA * log(h) + bA)) * cf
}

limit_CCC <- function(h, mC, bC, cf) {
  limit <- (exp(mC * log(h) + bC)) * cf
}

# Functions for calculating conversion factors for lead and cadmium (where the conversion factors are hardness-dependent)
# Lead: acute and chronic have the same formula but for cadmium, they differ

leadCF <- function(hardness) {
  cf <- 1.46203 - (log(hardness) * 0.145712)
}

cadCF_acute <- function(hardness) {
  cf <- 1.136672 - (log(hardness) * 0.041838)
}

cadCF_chronic <- function(hardness) {
  1.101672 - (log(hardness) * 0.041838)
}










