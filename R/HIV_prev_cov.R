#' Calculate HIV prevalence and HIV testing coverage
#'
#' Calculates HIV prevalence and HIV testing coverage stratified by snu1, year and/or reporting period.
#'
#' This function has been developed to calculate HIV prevalence and HIV testing coverage from ANC-RT data.
#' The HIV prevalence and testing coverage can be reported stratified by reporting period, year and/or subnational unit 1, 
#' according to user inputs.
#'
#' @param Data The ANC-RT dataset.  Ideally the functions \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} have been run on the data to properly
#' prepare the data for use here.  The dataset must have the following variables:
#'  \itemize{
#'   \item \code{snu1}: The subnational unit 1.
#'   \item \code{Time}: The time period that the data was collected (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{Year}: The year that the data was collected.
#'   \item \code{n_clients}: The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'   \item \code{TotPos}: Total number of positive HIV cases (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{n_stat}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} function).
#'     }
#' @param bysnu1 "TRUE" indicates that the results be stratified by snu1.  "FALSE" indicates that the results not be stratified.
#' @param byperiod "TRUE" indicates that the results be stratified by reporting period.  "FALSE" indicates that the results not be stratified.
#' @param byyear "TRUE" indicates that the results be stratified by year.  "FALSE" indicates that the results not be stratified.
#'
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A table (in dataframe format) indicating the results, stratified according to user inputs.  The results table includes the following columns:
#'  \itemize{
#'   \item{snu1} (only if results were stratified by snu1): The subnational unit 1.  
#'   \item{Time} (only if results were stratified by period and/or year): The time frame.  When results are stratified by year and reporting period, the digits "99" are suffixed to the year 
#'   (i.e. "201599" refers to the yearly result for 2015).
#'   \item{HIVprev}: The HIV prevalence adjusted for all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'   \item{HIVcov}: The HIV testing coverage adjusted for all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'  }
#'
#' @export
#' 

HIV_prev_cov <- function(Data, byperiod = "FALSE", bysnu1 = "FALSE", byyear = "FALSE"){
  
  HIV_prev <- round(((weighted.mean((Data$TotPos) / Data$n_stat, w = Data$n_stat, na.rm = TRUE)) * 100),2)
  HIV_cov <- round(((weighted.mean((Data$n_stat) / Data$n_clients, w = Data$n_clients, na.rm = TRUE)) * 100),2)
  
  HIVprevs <- function(Data){
    prev <- round(((weighted.mean((Data$TotPos) / Data$n_stat, w = Data$n_stat, na.rm = TRUE)) * 100),2)
    cov <- round(((weighted.mean((Data$n_stat) / Data$n_clients, w = Data$n_clients, na.rm = TRUE)) * 100),2)
    return(c(prev, cov))
  }
  prev_Time_snu <- ddply(Data, c("snu1", "Time"), HIVprevs)
  prev_Time_snu$HIVprev <- prev_Time_snu$V1
  prev_Time_snu$HIVcov <- prev_Time_snu$V2
  prev_Time_snu$V1 <- prev_Time_snu$V2 <- NULL
  
  prev_snu <- ddply(Data, "snu1", HIVprevs)
  prev_snu$HIVprev <- prev_snu$V1
  prev_snu$HIVcov <- prev_snu$V2
  prev_snu$V1 <- prev_snu$V2 <- NULL
  
  prev_Time <- ddply(Data, "Time", HIVprevs)
  prev_Time$HIVprev <- prev_Time$V1
  prev_Time$HIVcov <- prev_Time$V2
  prev_Time$V1 <- prev_Time$V2 <- NULL
  
  prev_year_snu <- ddply(Data, c("snu1", "Year"), HIVprevs)
  prev_year_snu$HIVprev <- prev_year_snu$V1
  prev_year_snu$HIVcov <- prev_year_snu$V2
  prev_year_snu$V1 <- prev_year_snu$V2 <- NULL
  
  prev_year <- ddply(Data, "Year", HIVprevs)
  prev_year$HIVprev <- prev_year$V1
  prev_year$HIVcov <- prev_year$V2
  prev_year$V1 <- prev_year$V2 <- NULL
  
  if (bysnu1 == "FALSE" & byperiod == "FALSE" & byyear == "FALSE"){
    return(c(HIV_prev, HIV_cov))
  }
  
  if (bysnu1 == "TRUE" & byperiod == "TRUE" & byyear == "TRUE"){
    prev_year_snu$Time <- prev_year_snu$Year
    prev_year_snu$Year <- NULL
    prev_year_snu$Time <- paste(prev_year_snu$Time, 99, sep = "")
    result <- rbind(prev_year_snu, prev_Time_snu)
    return(result)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "FALSE" & byyear == "FALSE"){
    return(prev_snu)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "TRUE" & byyear == "FALSE"){
    return(prev_Time)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "FALSE" & byyear == "TRUE"){
    return(prev_year)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "TRUE" & byyear == "FALSE"){
    return(prev_Time_snu)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "FALSE" & byyear == "TRUE"){
    return(prev_year_snu)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "TRUE" & byyear == "TRUE"){
    prev_year$Time <- prev_year$Year
    prev_year$Year <- NULL
    prev_year$Time <- paste(prev_year$Time, 99, sep = "")
    result2 <- rbind(prev_year, prev_Time)
    return(result2)
  }
}