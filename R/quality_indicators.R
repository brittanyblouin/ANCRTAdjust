quality <- function(data) {
  
  data$totpos_raw <- ifelse(!is.na(data$knownpos) & !is.na(data$testpos), data$knownpos + data$testpos, 
                            data$totpos)
  data$cov_raw <- ifelse(data$n_clients > 0 & !is.na(data$n_clients), (data$n_status / data$n_clients), NA)
  data$prv_raw <- ifelse(data$n_status > 0 & !is.na(data$n_status), (data$totpos_raw) / data$n_status, NA)
  data$cov <- ifelse(data$n_clients > 0 & !is.na(data$n_clients), (data$n_status_c / data$n_clients), NA)
  data$prv <- ifelse(data$n_status_c > 0 & !is.na(data$n_status_c), (data$totpos_c) / data$n_status_c, NA)
  
  ############################
  ##1. Quarters not reported##
  ############################
  Data <- data
  Data$NoData <- ifelse(is.na(Data$n_clients) & (is.na(Data$n_status) | Data$n_status == 0) & (is.na(Data$testpos) | Data$testpos == 0) 
                        & (is.na(Data$testneg) | Data$testneg == 0) & (is.na(Data$knownpos) | Data$knownpos == 0) & (is.na(Data$totpos) | Data$totpos == 0), 1, 0)
  Data <- Data[Data$NoData == 0,]
  Data$Ones <- 1
  quarters <- data.frame(Data$faciluid, Data$time, Data$Ones)
  wide <- reshape(quarters, idvar = "Data.faciluid", timevar = "Data.time", direction = "wide")
  wide[is.na(wide)] <- 0
  
  wide$NumVisits <- rowSums(wide[2:dim(wide)[2]])
  wide$keep <- ifelse(wide$NumVisits == length(unique(data$time)), 1, 0)
  
  o1 <- sum(ifelse(wide$keep == 0,1,0)) 
  op1 <- paste("(", round((1 - mean(wide$keep)) * 100, 2), "%)", sep = "")
  c1 <- "NA" 
  cp1 <- "NA"  
  
  ##################
  #2. Missing Data##
  ##################
  
  o2 <- sum(is.na(data$n_clients))
  op2 <- paste("(", round((sum(is.na(data$n_clients))/dim(data)[1]) * 100, 2), "%)", sep = "")
  c2 <- sum(is.na(data$n_clients)) 
  cp2 <- paste("(", round((sum(is.na(data$n_clients))/dim(data)[1]) * 100, 2), "%)", sep = "")
  
  o3 <- sum(is.na(data$n_status))
  op3 <- paste("(", round((sum(is.na(data$n_status))/dim(data)[1]) * 100, 2), "%)", sep = "")
  c3 <- sum(is.na(data$n_status_c))
  cp3 <- paste("(", round((sum(is.na(data$n_status_c))/dim(data)[1]) * 100, 2), "%)", sep = "")
  
  o4 <- sum(is.na(data$testpos))
  op4 <- paste("(", round((sum(is.na(data$testpos))/dim(data)[1]) * 100, 2), "%)", sep = "")
  c4 <- sum(is.na(data$testpos_c))
  cp4 <- paste("(", round((sum(is.na(data$testpos_c))/dim(data)[1]) * 100, 2), "%)", sep = "")
  
  o5 <- sum(is.na(data$testneg))
  op5 <- paste("(", round((sum(is.na(data$testneg))/dim(data)[1]) * 100, 2), "%)", sep = "")
  c5 <- sum(is.na(data$testneg_c))
  cp5 <- paste("(", round((sum(is.na(data$testneg_c))/dim(data)[1]) * 100, 2), "%)", sep = "")
  
  o6 <- sum(is.na(data$knownpos))
  op6 <- paste("(", round((sum(is.na(data$knownpos))/dim(data)[1]) * 100, 2), "%)", sep = "")
  c6 <- sum(is.na(data$knownpos_c))
  cp6 <- paste("(", round((sum(is.na(data$knownpos_c))/dim(data)[1]) * 100, 2), "%)", sep = "")
  

  missing <- subset(data, select = c('n_clients', 'n_status', 'totpos_raw'))
  o7 <- (dim(missing)[1] - dim(na.omit(missing))[1])
  op7 <- paste("(", round(((dim(missing)[1] - dim(na.omit(missing))[1]) / dim(missing)[1]) * 100, 2), "%)", sep = "")
  missing2 <- subset(data, select = c('n_clients', 'n_status_c', 'totpos_c'))
  c7 <- (dim(missing2)[1] - dim(na.omit(missing2))[1])
  cp7 <- paste("(", round(((dim(missing2)[1] - dim(na.omit(missing2))[1]) / dim(missing2)[1]) * 100, 2), "%)", sep = "")
  
  #######################
  ##1 Impossible Values##
  #######################
  o8 <- sum(ifelse(data$cov_raw > 1, 1, 0), na.rm = TRUE)
  op8 <- paste("(", round((sum(ifelse(data$cov_raw > 1, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  c8 <- sum(ifelse(data$cov > 1, 1, 0), na.rm = TRUE)
  cp8 <- paste("(", round((sum(ifelse(data$cov > 1, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  
  o9 <- sum(ifelse(data$prv_raw > 1, 1, 0), na.rm = TRUE)
  op9 <- paste("(", round((sum(ifelse(data$prv_raw > 1, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  c9 <- sum(ifelse(data$prv > 1, 1, 0), na.rm = TRUE)
  cp9 <- paste("(", round((sum(ifelse(data$prv > 1, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100 ,2), "%)", sep = "")
  
  o10 <- sum(ifelse(data$n_status < (data$testneg + data$testpos + data$knownpos), 1, 0), na.rm = TRUE)
  op10 <- paste("(", round((sum(ifelse(data$n_status < (data$testneg + data$testpos + data$knownpos), 1, 0), na.rm = TRUE)/
                              dim(data)[1]) * 100, 2), "%)", sep = "")
  c10 <- sum(ifelse(data$n_status_c < (data$testneg_c + data$testpos_c + data$knownpos_c), 1, 0), na.rm = TRUE)
  cp10 <- paste("(", round((sum(ifelse(data$n_status_c < (data$testneg_c + data$testpos_c + data$knownpos_c), 1, 0), na.rm = TRUE)/
                              dim(data)[1]) * 100, 2), "%)", sep = "")
  
  o11 <- sum(ifelse(data$n_clients < 0, 1, 0), na.rm = TRUE)
  op11 <- paste("(", round((sum(ifelse(data$n_clients<0, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  c11 <- sum(ifelse(data$n_clients < 0, 1, 0), na.rm = TRUE)
  cp11 <- paste("(", round((sum(ifelse(data$n_clients < 0, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  
  o12 <- sum(ifelse(data$n_status < 0, 1, 0), na.rm = TRUE)
  op12 <- paste("(", round((sum(ifelse(data$n_status<0,1,0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  c12 <- sum(ifelse(data$n_status_c < 0, 1, 0), na.rm = TRUE)
  cp12 <- paste("(", round((sum(ifelse(data$n_status_c < 0, 1, 0), na.rm  =TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  
  o13 <- sum(ifelse(data$testpos < 0, 1, 0), na.rm = TRUE)
  op13 <- paste("(", round((sum(ifelse(data$testpos < 0, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  c13 <- sum(ifelse(data$testpos_c < 0, 1, 0), na.rm = TRUE)
  cp13 <- paste("(", round((sum(ifelse(data$testpos_c < 0, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  
  o14 <- sum(ifelse(data$testneg < 0, 1, 0), na.rm = TRUE)
  op14 <- paste("(", round((sum(ifelse(data$testneg < 0, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  c14 <- sum(ifelse(data$testneg_c < 0, 1, 0), na.rm = TRUE)
  cp14 <- paste("(", round((sum(ifelse(data$testneg_c < 0, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  
  o15 <- sum(ifelse(data$knownpos < 0, 1, 0), na.rm = TRUE)
  op15 <- paste("(", round((sum(ifelse(data$knownpos < 0, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  c15 <- sum(ifelse(data$knownpos_c < 0, 1, 0), na.rm = TRUE)
  cp15 <- paste("(", round((sum(ifelse(data$knownpos_c < 0, 1, 0), na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  
  data$impossible_raw <- ifelse(data$cov_raw > 1 & !is.na(data$cov_raw), 1, 
                                 ifelse(data$n_status < (data$testneg + data$testpos + data$knownpos) & !is.na(data$n_status) & !is.na(data$testneg) & !is.na(data$testpos) & !is.na(data$knownpos), 1,
                                        ifelse(data$n_clients < 0 & !is.na(data$n_clients), 1,
                                               ifelse(data$n_status < 0 & !is.na(data$n_status), 1,
                                                      ifelse(data$testpos < 0 & !is.na(data$testpos), 1,
                                                             ifelse(data$knownpos < 0 & !is.na(data$knownpos), 1, 0))))))
  data$impossible_cleaned <- ifelse(data$cov > 1 & !is.na(data$cov), 1, 
                                     ifelse(data$n_status_c < (data$testneg_c + data$testpos_c + data$knownpos_c) & !is.na(data$n_status_c) & !is.na(data$testneg_c) & !is.na(data$testpos_c) & !is.na(data$knownpos_c), 1,
                                            ifelse(data$n_clients < 0 & !is.na(data$n_clients), 1,
                                                   ifelse(data$n_status_c < 0 & !is.na(data$n_status_c), 1,
                                                          ifelse(data$testpos_c < 0 & !is.na(data$testpos_c), 1,
                                                                 ifelse(data$knownpos_c < 0 & !is.na(data$knownpos_c), 1, 0))))))
  
  o16 <- sum(data$impossible_raw, na.rm = TRUE)
  op16 <- paste("(", round((sum(data$impossible_raw, na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  c16 <- sum(data$impossible_cleaned, na.rm = TRUE)
  cp16 <- paste("(", round((sum(data$impossible_cleaned, na.rm = TRUE) / dim(data)[1]) * 100, 2), "%)", sep = "")
  
  table <- data.frame(
     Raw.Count = c(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16),
     Raw.Percent = c(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12, op13, op14, op15, op16),
     Clean.Count = c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16),
     Clean.Percent = c(cp1, cp2, cp3, cp4, cp5, cp6, cp7, cp8, cp9, cp10, cp11, cp12, cp13, cp14, cp15, cp16), stringsAsFactors = FALSE)
  row.names(table) <- c("Missing >=1 quarter", "Missing n_clients", "Missing n_status", "Missing testpos", "Missing testneg", "Missing knownpos", "Missing >=1 variables",
                        "Invalid coverage", "Invalid prevalence", "Inconsistent n_status", "Negative n_clients", "Negative n_status", "Negative testpos", "Negative testneg", 
                        "Negative knownpos", "One or more invalid variables")
  return(table)
}


#' Data Quality Indicators
#'
#' Calculates the different data quality indicators regarding missing data and invalid data from ANC-RT data.
#'
#' This function was designed to calculate 16 data quality indicators from ANC-RT data.  The amount of missing data and invalid data for the 
#' five primary variables used to calculate HIV testing coverage and HIV prevalence (i.e. \code{n_clients}, \code{n_status}, \code{testpos}, 
#' \code{testneg} and, \code{knownpos}) are calculated and output in a dataframe. Data quality indicators are calculated from both raw and cleaned
#' variables and can be calculated stratified by region and time, according to user inputs.
#' 
#' @param data The ANC-RT dataset.  The functions \link[ANCRTAdjust]{name_var} and \link[ANCRTAdjust]{data_clean} should have been run on the data to properly
#' prepare the data for use here.  The data set must have the following variables:
#'  \itemize{
#'   \item \code{faciluid}: The facility identification code
#'   \item \code{time}: The time period
#'   \item \code{snu1}: The sub-national unit 1 (only necessary if results are to be stratified by region)
#'   \item \code{n_clients}: The number of women who attended the specific facility during the specific time period for their first ANC visit
#'   \item \code{n_status}: The number of women who attended the specific facility during the specific time period for their first ANC visit and had their HIV status ascertained 
#'   \item \code{testpos}: The number of women who tested positive for HIV at their first ANC visit at the specific facility during the specific time period
#'   \item \code{testneg}: The number of women who tested negative for HIV at their first ANC visit at the specific facility during the specific time period
#'   \item \code{knownpos}: The number of women who attended their first ANC visit at the specific facility during the specific time period with previous knowledge of being HIV positive
#'   \item \code{totpos}: The number of women who attended their first ANC visit at the specific facility during the specific time period who were HIV-positive
#'   \item \code{n_status_c}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{testpos_c}: Cleaned \code{testpos} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{testneg_c}: Cleaned \code{testneg} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{knownpos_c}: Cleaned \code{knownpos} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{totpos_c}: Cleaned \code{totpos} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   }
#' @param by_region TRUE or FALSE to indicate whether the data quality indicators be calculated stratified by \code{snu1}
#' @param by_time TRUE or FALSE to indicate whether the data quality indicators be calculated stratified by \code{time}
#' 
#' @import stats
#' 
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A table (in dataframe format) with all data quality indicators calculated for the raw and cleaned data.  If specified, data quality 
#' indicators are also displayed stratified by \code{snu1} and/or \code{time}.  The 16 data quality indicators include:
#' \itemize{
#'   \item The number and \% of facilities that don't report all quarters
#'   \item The number and \% of observations with missing data for \code{n_clients}
#'   \item The number and \% of observations with missing data for \code{n_status} 
#'   \item The number and \% of observations with missing data for \code{testpos} 
#'   \item The number and \% of observations with missing data for \code{testneg} 
#'   \item The number and \% of observations with missing data for \code{knownpos}
#'   \item The number and \% of observations with missing data for at least one of  \code{n_clients},  \code{n_status}, or \code{totpos}
#'   \item The number and \% of observations with \code{n_status} > \code{n_clients}
#'   \item The number and \% of observations with \code{n_status} < \code{totpos}
#'   \item The number and \% of observations with \code{n_status} < (\code{testpos} + \code{testneg} + \code{knownpos})
#'   \item The number and \% of observations with negative values for \code{n_clients}
#'   \item The number and \% of observations with negative values for \code{n_status}
#'   \item The number and \% of observations with negative values for \code{testpos}
#'   \item The number and \% of observations with negative values for \code{testneg}
#'   \item The number and \% of observations with negative values for \code{knownpos}
#'   \item The number and \% of observations with invalid data for at least one of \code{n_clients},  \code{n_status}, \code{testpos}, or \code{knownpos}
#'}
#' @export

quality_indicators <- function(data, by_region = FALSE, by_time = FALSE){
  if (by_region == FALSE & by_time == FALSE){
    table <- quality(data)
    return(table)
  }
  
  if (by_region == TRUE & by_time == FALSE){
    table <- quality(data)
    table$region <- "All"
    row.names(table) <- c("Missing >=1 quarter", "Missing n_clients", "Missing n_status", "Missing testpos", "Missing testneg", "Missing knownpos", "Missing >=1 variables",
                          "Invalid coverage", "Invalid prevalence", "Inconsistent n_status", "Negative n_clients", "Negative n_status", "Negative testpos", "Negative testneg", 
                          "Negative knownpos", "One or more invalid variables")
    
    
    table2 <- NULL
    region.list <- (unique(data$snu1))
    for (i in 1:length(region.list)) {
      RegionData <- data[data$snu1 == region.list[i],]
      table3 <- quality(RegionData)
      table3$region <- region.list[i]
      row.names(table3)<-c(paste("Missing >=1 quarter", i, sep = '.'), paste("Missing n_clients", i, sep = '.'), paste("Missing n_status", i, sep = '.'), paste("Missing testpos", i, sep = '.'), paste("Missing testneg", i, sep = '.'),
                           paste("Missing knownpos", i, sep = '.'), paste("Missing >=1 variables", i, sep = '.'), paste("Invalid coverage", i, sep = '.'), paste("Invalid prevalence", i, sep = '.'),
                           paste("Inconsistent n_status", i, sep = '.'), paste("Negative n_clients", i, sep = '.'), paste("Negative n_status", i, sep = '.'), paste("Negative testpos", i, sep = '.'), paste("Negative testneg", i, sep = '.'),
                           paste("Negative knownpos", i, sep = '.'), paste("One or more invalid variables", i, sep = '.'))
      table2 <- rbind(table2, table3)
    }
    table <- rbind(table, table2)
    return(table)
  }

  if (by_region == FALSE & by_time == TRUE) {
    table <- quality(data)
    table$time <- "All"
    row.names(table) <- c("Missing >=1 quarter", "Missing n_clients", "Missing n_status", "Missing testpos", "Missing testneg", "Missing knownpos", "Missing >=1 variables",
                          "Invalid coverage", "Invalid prevalence", "Inconsistent n_status", "Negative n_clients", "Negative n_status", "Negative testpos", "Negative testneg", 
                          "Negative knownpos", "One or more invalid variables")
    
    
    table2 <- NULL
    time.list <- (unique(data$time))
    for (i in 1:length(time.list)) {
      TimeData <- data[data$time == time.list[i],]
      table3 <- quality(TimeData)
      table3[1,1] <- table3[1,2] <- "NA"
      table3$time <- time.list[i]
      row.names(table3)<-c(paste("Missing >=1 quarter", i, sep = '.'), paste("Missing n_clients", i, sep = '.'), paste("Missing n_status", i, sep = '.'), paste("Missing testpos", i, sep = '.'), paste("Missing testneg", i, sep = '.'),
                           paste("Missing knownpos", i, sep = '.'), paste("Missing >=1 variables", i, sep = '.'), paste("Invalid coverage", i, sep = '.'), paste("Invalid prevalence", i, sep = '.'),
                           paste("Inconsistent n_status", i, sep = '.'), paste("Negative n_clients", i, sep = '.'), paste("Negative n_status", i, sep = '.'), paste("Negative testpos", i, sep = '.'), paste("Negative testneg", i, sep = '.'),
                           paste("Negative knownpos", i, sep = '.'), paste("One or more invalid variables", i, sep = '.'))
      table2 <- rbind(table2, table3)
    }
    table <- rbind(table, table2)
    return(table)
  }
  
  if (by_region == TRUE & by_time == TRUE) {
    table <- quality(data)
    table$time <- "All"
    table$region <- "All"
    row.names(table) <- c("Missing >=1 quarter", "Missing n_clients", "Missing n_status", "Missing testpos", "Missing testneg", "Missing knownpos", "Missing >=1 variables",
                          "Invalid coverage", "Invalid prevalence", "Inconsistent n_status", "Negative n_clients", "Negative n_status", "Negative testpos", "Negative testneg", 
                          "Negative knownpos", "One or more invalid variables")
    
    
    table2 <- NULL
    data$snu1.time <- paste(data$snu1, data$time, sep = "-")
    timeregion.list <- (unique(data$snu1.time))
    for (i in 1:length(timeregion.list)) {
      Data <- data[data$snu1.time == timeregion.list[i],]
      table3 <- quality(Data)
      table3[1,1] <- table3[1,2] <- "NA"
      table3$time <- Data$time[1]
      table3$region <- Data$snu1[1]
      row.names(table3)<-c(paste("Missing >=1 quarter", i, sep = '.'), paste("Missing n_clients", i, sep = '.'), paste("Missing n_status", i, sep = '.'), paste("Missing testpos", i, sep = '.'), paste("Missing testneg", i, sep = '.'),
                           paste("Missing knownpos", i, sep = '.'), paste("Missing >=1 variables", i, sep = '.'), paste("Invalid coverage", i, sep = '.'), paste("Invalid prevalence", i, sep = '.'),
                           paste("Inconsistent n_status", i, sep = '.'), paste("Negative n_clients", i, sep = '.'), paste("Negative n_status", i, sep = '.'), paste("Negative testpos", i, sep = '.'), paste("Negative testneg", i, sep = '.'),
                           paste("Negative knownpos", i, sep = '.'), paste("One or more invalid variables", i, sep = '.'))
      table2 <- rbind(table2, table3)
    }
    table <- rbind(table, table2)
    return(table)
  }
  
}

