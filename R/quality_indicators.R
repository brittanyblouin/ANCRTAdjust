quality <- function(Data1){
  
  Data1$totpos_raw <- ifelse(!is.na(Data1$knownpos) & !is.na(Data1$testpos), Data1$knownpos + Data1$testpos, 
                            Data1$totpos)
  Data1$Cov_raw <- ifelse(Data1$n_clients>0 & !is.na(Data1$n_clients), (Data1$n_status / Data1$n_clients), NA)
  Data1$Prv_raw <- ifelse(Data1$n_status>0 & !is.na(Data1$n_status), (Data1$totpos_raw) / Data1$n_status, NA)
  Data1$Cov <- ifelse(Data1$n_clients>0 & !is.na(Data1$n_clients), (Data1$n_stat / Data1$n_clients), NA)
  Data1$Prv <- ifelse(Data1$n_stat>0 & !is.na(Data1$n_stat), (Data1$TotPos) / Data1$n_stat, NA)
  
  ############################
  ##1. Quarters not reported##
  ############################
  Data <- Data1
  Data$NoData <- ifelse(is.na(Data$n_clients) & (is.na(Data$n_status) | Data$n_status == 0) & (is.na(Data$testpos) | Data$testpos == 0) 
                        & (is.na(Data$testneg) | Data$testneg == 0) & (is.na(Data$knownpos) | Data$knownpos == 0) & (is.na(Data$totpos) | Data$totpos == 0), 1, 0)
  Data <- Data[Data$NoData == 0,]
  Data$Ones <- 1
  quarters <- data.frame(Data$ID, Data$period, Data$Ones)
  wide <- reshape(quarters, idvar = "Data.ID", timevar = "Data.period", direction = "wide")
  wide[is.na(wide)] <- 0
  
  wide$NumVisits <- rowSums(wide[2:dim(wide)[2]])
  wide$keep <- ifelse(wide$NumVisits == length(unique(Data1$period)), 1, 0)
  
  o1 <- sum(ifelse(wide$keep==0,1,0)) 
  op1 <- paste("(", round((1-mean(wide$keep)) * 100, 2), "%)", sep = "")
  c1 <- "NA" 
  cp1 <- "NA"  
  
  ##################
  #2. Missing Data##
  ##################
  
  o2 <- sum(is.na(Data1$n_clients))
  op2 <- paste("(", round((sum(is.na(Data1$n_clients))/dim(Data1)[1])*100,2), "%)", sep = "")
  c2 <- sum(is.na(Data1$n_clients)) 
  cp2 <- paste("(", round((sum(is.na(Data1$n_clients))/dim(Data1)[1])*100,2), "%)", sep = "")
  
  o3 <- sum(is.na(Data1$n_status))
  op3 <- paste("(", round((sum(is.na(Data1$n_status))/dim(Data1)[1])*100,2), "%)", sep = "")
  c3 <- sum(is.na(Data1$n_stat))
  cp3 <- paste("(", round((sum(is.na(Data1$n_stat))/dim(Data1)[1])*100,2), "%)", sep = "")
  
  o4 <- sum(is.na(Data1$testpos))
  op4 <- paste("(", round((sum(is.na(Data1$testpos))/dim(Data1)[1])*100,2), "%)", sep = "")
  c4 <- sum(is.na(Data1$TestPos))
  cp4 <- paste("(", round((sum(is.na(Data1$TestPos))/dim(Data1)[1])*100,2), "%)", sep = "")
  
  o5 <- sum(is.na(Data1$testneg))
  op5 <- paste("(", round((sum(is.na(Data1$testneg))/dim(Data1)[1])*100,2), "%)", sep = "")
  c5 <- sum(is.na(Data1$TestNeg))
  cp5 <- paste("(", round((sum(is.na(Data1$TestNeg))/dim(Data1)[1])*100,2), "%)", sep = "")
  
  o6 <- sum(is.na(Data1$knownpos))
  op6 <- paste("(", round((sum(is.na(Data1$knownpos))/dim(Data1)[1])*100,2), "%)", sep = "")
  c6 <- sum(is.na(Data1$KnownPos))
  cp6 <- paste("(", round((sum(is.na(Data1$KnownPos))/dim(Data1)[1])*100,2), "%)", sep = "")
  
  missing <- subset(Data1, select=c('n_clients', 'n_status', 'totpos_raw'))
  o7 <- (dim(missing)[1] - dim(na.omit(missing))[1])
  op7 <- paste("(", round(((dim(missing)[1] - dim(na.omit(missing))[1])/dim(missing)[1])*100,2), "%)", sep = "")
  missing2 <- subset(Data1, select=c('n_clients', 'n_stat', 'TotPos'))
  c7 <- (dim(missing2)[1] - dim(na.omit(missing2))[1])
  cp7 <- paste("(", round(((dim(missing2)[1] - dim(na.omit(missing2))[1])/dim(missing2)[1])*100,2), "%)", sep = "")
  
  #######################
  ##1 Impossible Values##
  #######################
  o8 <- sum(ifelse(Data1$Cov_raw > 1, 1, 0), na.rm = TRUE)
  op8 <- paste("(", round((sum(ifelse(Data1$Cov_raw > 1, 1, 0), na.rm = TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  c8 <- sum(ifelse(Data1$Cov > 1, 1, 0), na.rm = TRUE)
  cp8 <- paste("(", round((sum(ifelse(Data1$Cov > 1, 1, 0), na.rm = TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  
  o9 <- sum(ifelse(Data1$Prv_raw > 1, 1, 0), na.rm = TRUE)
  op9 <- paste("(", round((sum(ifelse(Data1$Prv_raw > 1, 1, 0), na.rm = TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  c9 <- sum(ifelse(Data1$Prv > 1, 1, 0), na.rm = TRUE)
  cp9 <- paste("(", round((sum(ifelse(Data1$Prv > 1, 1, 0), na.rm = TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  
  o10 <- sum(ifelse(Data1$n_status < (Data1$testneg + Data1$testpos + Data1$knownpos), 1, 0), na.rm = TRUE)
  op10 <- paste("(", round((sum(ifelse(Data1$n_status < (Data1$testneg + Data1$testpos + Data1$knownpos), 1, 0), na.rm = TRUE)/
                              dim(Data1)[1])*100,2), "%)", sep = "")
  c10 <- sum(ifelse(Data1$n_stat < (Data1$TestNeg + Data1$TestPos + Data1$KnownPos), 1, 0), na.rm = TRUE)
  cp10 <- paste("(", round((sum(ifelse(Data1$n_stat < (Data1$TestNeg + Data1$TestPos + Data1$KnownPos), 1, 0), na.rm = TRUE)/
                              dim(Data1)[1])*100,2), "%)", sep = "")
  
  o11 <- sum(ifelse(Data1$n_clients<0,1,0), na.rm=TRUE)
  op11 <- paste("(", round((sum(ifelse(Data1$n_clients<0,1,0), na.rm=TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  c11 <- sum(ifelse(Data1$n_clients<0,1,0), na.rm=TRUE)
  cp11 <- paste("(", round((sum(ifelse(Data1$n_clients<0,1,0), na.rm=TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  
  o12 <- sum(ifelse(Data1$n_status<0,1,0), na.rm=TRUE)
  op12 <- paste("(", round((sum(ifelse(Data1$n_status<0,1,0), na.rm=TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  c12 <- sum(ifelse(Data1$n_stat<0,1,0), na.rm=TRUE)
  cp12 <- paste("(", round((sum(ifelse(Data1$n_stat<0,1,0), na.rm=TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  
  o13 <- sum(ifelse(Data1$testpos<0,1,0), na.rm=TRUE)
  op13 <- paste("(", round((sum(ifelse(Data1$testpos<0,1,0), na.rm=TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  c13 <- sum(ifelse(Data1$TestPos<0,1,0), na.rm=TRUE)
  cp13 <- paste("(", round((sum(ifelse(Data1$TestPos<0,1,0), na.rm=TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  
  o14 <- sum(ifelse(Data1$testneg<0,1,0), na.rm=TRUE)
  op14 <- paste("(", round((sum(ifelse(Data1$testneg<0,1,0), na.rm=TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  c14 <- sum(ifelse(Data1$TestNeg<0,1,0), na.rm=TRUE)
  cp14 <- paste("(", round((sum(ifelse(Data1$TestNeg<0,1,0), na.rm=TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  
  o15 <- sum(ifelse(Data1$knownpos<0,1,0), na.rm=TRUE)
  op15 <- paste("(", round((sum(ifelse(Data1$knownpos<0,1,0), na.rm=TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  c15 <- sum(ifelse(Data1$KnownPos<0,1,0), na.rm=TRUE)
  cp15 <- paste("(", round((sum(ifelse(Data1$KnownPos<0,1,0), na.rm=TRUE)/dim(Data1)[1])*100,2), "%)", sep = "")
  
  Data1$impossible_raw <- ifelse(Data1$Cov_raw > 1 & !is.na(Data1$Cov_raw), 1, 
                                 ifelse(Data1$n_status < (Data1$testneg + Data1$testpos + Data1$knownpos) & !is.na(Data1$n_status) & !is.na(Data1$testneg) & !is.na(Data1$testpos) & !is.na(Data1$knownpos), 1,
                                        ifelse(Data1$n_clients < 0 & !is.na(Data1$n_clients), 1,
                                               ifelse(Data1$n_status < 0 & !is.na(Data1$n_status), 1,
                                                      ifelse(Data1$testpos < 0 & !is.na(Data1$testpos), 1,
                                                             ifelse(Data1$knownpos < 0 & !is.na(Data1$knownpos), 1, 0))))))
  Data1$impossible_cleaned <- ifelse(Data1$Cov > 1 & !is.na(Data1$Cov), 1, 
                                     ifelse(Data1$n_stat < (Data1$TestNeg + Data1$TestPos + Data1$KnownPos) & !is.na(Data1$n_stat) & !is.na(Data1$TestNeg) & !is.na(Data1$TestPos) & !is.na(Data1$KnownPos), 1,
                                            ifelse(Data1$n_clients < 0 & !is.na(Data1$n_clients), 1,
                                                   ifelse(Data1$n_stat < 0 & !is.na(Data1$n_stat), 1,
                                                          ifelse(Data1$TestPos < 0 & !is.na(Data1$TestPos), 1,
                                                                 ifelse(Data1$KnownPos < 0 & !is.na(Data1$KnownPos), 1, 0))))))
  
  o16 <- sum(Data1$impossible_raw, na.rm = TRUE)
  op16 <- paste("(", round((sum(Data1$impossible_raw, na.rm = TRUE) / dim(Data1)[1])*100,2), "%)", sep = "")
  c16 <- sum(Data1$impossible_cleaned, na.rm = TRUE)
  cp16 <- paste("(", round((sum(Data1$impossible_cleaned, na.rm = TRUE) / dim(Data1)[1])*100,2), "%)", sep = "")
  
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
#' Calculates the different data quality indicators due to missing data and invalid data from ANC-RT data.
#'
#' This function was designed to calculate 16 data quality indicators from ANC-RT data.  The amount of missing data and invalid data for the 
#' five primary variables used to calculate HIV testing coverage and HIV prevalence (i.e. \code{n_clients}, \code{n_status}, \code{testpos}, 
#' \code{testneg} and, \code{knownpos} are calculated and output in a dataframe. Data quality indicators are calculated from both raw and cleaned
#' variables and can be calculated stratified by region and time, according to user inputs.
#' 
#' @param Data1 The ANC-RT dataset.  Ideally the function \link[ANCRTAdjust]{data_clean} has been run on the data to properly
#' prepare the data for use here.  The data set must have the following variables:
#'  \itemize{
#'   \item \code{faciluid}: The facility identification code
#'   \item \code{period}: The time period
#'   \item \code{snu1}: The sub-national unit 1
#'   \item \code{Time}: Cleaned \code{period} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_clients}: The number of women who attended the specific facility during the specific time period for their first ANC visit
#'   \item \code{n_status}: The number of women who attended the specific facility during the specific time period for their first ANC visit and had their HIV status ascertained 
#'   \item \code{testpos}: The number of women who tested positive for HIV at their first ANC visit at the specific facility during the specific time period
#'   \item \code{testneg}: The number of women who tested negative for HIV at their first ANC visit at the specific facility during the specific time period
#'   \item \code{knownpos}: The number of women who attended their first ANC visit at the specific facility during the specific time period with previous knowledge of being HIV positive
#'   \item \code{n_stat}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{TestPos}: Cleaned \code{testpos} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{TestNeg}: Cleaned \code{testneg} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{KnownPos}: Cleaned \code{knownpos} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   }
#' @param byregion "TRUE" or "FALSE" to indicate whether the data quality indicators should be calculated stratified by \code{snu1}
#' @param bytime "TRUE" or "FALSE" to indicate whether the data quality indicators should be calculated stratified by \code{Time}
#' 
#' @import stats
#' 
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A table (in dataframe format) with all data quality indicators calculated for the raw and cleaned data.  If specified, data quality 
#' indicators are also displayed stratified by \code{snu1} and/or \code{Time}.  The 16 data quality indicators include:
#' \itemize{
#'   \item The number and \% of facilities that don't report all quarters
#'   \item The number and \% of observations with missing data for \code{n_clients}
#'   \item The number and \% of observations with missing data for \code{n_status} 
#'   \item The number and \% of observations with missing data for \code{testpos} 
#'   \item The number and \% of observations with missing data for \code{testneg} 
#'   \item The number and \% of observations with missing data for \code{knownpos}
#'   \item The number and \% of observations with missing data for at least one of  \code{n_clients},  \code{n_status}, \code{testpos}, or \code{knownpos}
#'   \item The number and \% of observations with \code{n_status} > \code{n_clients}
#'   \item The number and \% of observations with \code{n_status} < (\code{testpos} + \code{knownpos})
#'   \item The number and \% of observations with \code{n_status} < (\code{testpos} + \code{testneg} + \code{knownpos})
#'   \item The number and \% of observations with negative values for \code{n_clients}
#'   \item The number and \% of observations with negative values for \code{n_status}
#'   \item The number and \% of observations with negative values for \code{testpos}
#'   \item The number and \% of observations with negative values for \code{testneg}
#'   \item The number and \% of observations with negative values for \code{knownpos}
#'   \item The number and \% of observations with invalid data for at least one of \code{n_clients},  \code{n_status}, \code{testpos}, or \code{knownpos}
#'}
#' @export

quality_indicators <- function(Data1, byregion = "FALSE", bytime = "FALSE"){
  if (byregion == "FALSE" & bytime == "FALSE"){
    table <- quality(Data1)
    return(table)
  }
  
  if (byregion == "TRUE" & bytime == "FALSE"){
    table <- quality(Data1)
    table$region <- "All"
    row.names(table) <- c("Missing >=1 quarter", "Missing n_clients", "Missing n_status", "Missing testpos", "Missing testneg", "Missing knownpos", "Missing >=1 variables",
                          "Invalid coverage", "Invalid prevalence", "Inconsistent n_status", "Negative n_clients", "Negative n_status", "Negative testpos", "Negative testneg", 
                          "Negative knownpos", "One or more invalid variables")
    
    
    table2 <- NULL
    region.list <- (unique(Data1$snu1))
    for (i in 1:length(region.list)){
      RegionData <- Data1[Data1$snu1 == region.list[i],]
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

  if (byregion == "FALSE" & bytime == "TRUE"){
    table <- quality(Data1)
    table$time <- "All"
    row.names(table) <- c("Missing >=1 quarter", "Missing n_clients", "Missing n_status", "Missing testpos", "Missing testneg", "Missing knownpos", "Missing >=1 variables",
                          "Invalid coverage", "Invalid prevalence", "Inconsistent n_status", "Negative n_clients", "Negative n_status", "Negative testpos", "Negative testneg", 
                          "Negative knownpos", "One or more invalid variables")
    
    
    table2 <- NULL
    time.list <- (unique(Data1$Time))
    for (i in 1:length(time.list)){
      TimeData <- Data1[Data1$Time == time.list[i],]
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
  
  if (byregion == "TRUE" & bytime == "TRUE"){
    table <- quality(Data1)
    table$time <- "All"
    table$region <- "All"
    row.names(table) <- c("Missing >=1 quarter", "Missing n_clients", "Missing n_status", "Missing testpos", "Missing testneg", "Missing knownpos", "Missing >=1 variables",
                          "Invalid coverage", "Invalid prevalence", "Inconsistent n_status", "Negative n_clients", "Negative n_status", "Negative testpos", "Negative testneg", 
                          "Negative knownpos", "One or more invalid variables")
    
    
    table2 <- NULL
    Data1$snu1.time <- paste(Data1$snu1, Data1$Time, sep = "-")
    timeregion.list <- (unique(Data1$snu1.time))
    for (i in 1:length(timeregion.list)){
      Data <- Data1[Data1$snu1.time == timeregion.list[i],]
      table3 <- quality(Data)
      table3[1,1] <- table3[1,2] <- "NA"
      table3$time <- Data$Time[1]
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

