#' Calculate HIV prevalence and HIV testing coverage
#'
#' Calculates HIV prevalence and HIV testing coverage stratified by snu1, year and/or reporting period.
#'
#' This function has been developed to calculate HIV prevalence and HIV testing coverage from ANC-RT data.
#' The HIV prevalence and testing coverage can be reported stratified by reporting period, year and/or subnational unit 1, 
#' according to user inputs.
#'
#' @param data The ANC-RT dataset.  The functions \link[ANCRTAdjust]{name_var}, \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} should have been run on the data to properly
#' prepare the data for use here.  The dataset must have the following variables:
#'  \itemize{
#'   \item \code{n_clients}: The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'   \item \code{totpos_c}: Total number of positive HIV cases (generated using the \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} functions).
#'   \item \code{n_status_c}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} functions).
#'   \item \code{snu1}: The subnational unit 1 (only required if results are to be stratified by snu1).
#'   \item \code{time}: The time period that the data was collected (only required if results are to be stratified by the reporting time period).
#'   \item \code{Year}: The year that the data was collected (only required if results are to be stratified by year).
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
#'   \item{Time} (only if results were stratified by time period and/or year): The time frame.  When results are stratified by year and reporting period, the digits "99" are suffixed to the year 
#'   (i.e. "201599" refers to the yearly result for 2015).
#'   \item{HIVraw}: The raw HIV prevalence (i.e. with no previous adjustments).
#'   \item{HIVprev}: The HIV prevalence adjusted for all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'   \item{HIVcov}: The HIV testing coverage adjusted for all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'  }
#'
#' @export
#' 

HIV_prev_cov <- function(data, byperiod = "FALSE", bysnu1 = "FALSE", byyear = "FALSE"){
  
  HIVprevs <- function(data){
    raw <- round(((weighted.mean((data$totpos) / data$n_status, w = data$n_status, na.rm = TRUE)) * 100), 2)
    prev <- round(((weighted.mean((data$totpos_c) / data$n_status_c, w = data$n_status_c, na.rm = TRUE)) * 100), 2)
    cov <- round(((weighted.mean((data$n_status_c) / data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100), 2)
    return(c(raw, prev, cov))
  }
  
  if (bysnu1 == "FALSE" & byperiod == "FALSE" & byyear == "FALSE"){
    HIVraw <- round(((weighted.mean((data$totpos) / data$n_status, w = data$n_status, na.rm = TRUE)) * 100), 2)
    HIVprev <- round(((weighted.mean((data$totpos_c) / data$n_status_c, w = data$n_status_c, na.rm = TRUE)) * 100), 2)
    HIVcov <- round(((weighted.mean((data$n_status_c) / data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100), 2)
    snu1 <- "All"
    return(data.frame(snu1, HIVraw, HIVprev, HIVcov))
  }
  
  if (bysnu1 == "TRUE" & byperiod == "TRUE" & byyear == "TRUE"){
    prev_year_snu <- ddply(data, c("snu1", "Year"), HIVprevs)
    prev_year_snu$HIVraw <- prev_year_snu$V1
    prev_year_snu$HIVprev <- prev_year_snu$V2
    prev_year_snu$HIVcov <- prev_year_snu$V3
    prev_year_snu$V1 <- prev_year_snu$V2 <- prev_year_snu$V3 <- NULL
    
    prev_Time_snu <- ddply(data, c("snu1", "time"), HIVprevs)
    prev_Time_snu$HIVraw <- prev_Time_snu$V1
    prev_Time_snu$HIVprev <- prev_Time_snu$V2
    prev_Time_snu$HIVcov <- prev_Time_snu$V3
    prev_Time_snu$V1 <- prev_Time_snu$V2 <- prev_Time_snu$V3 <- NULL
    
    prev_year_snu$time <- prev_year_snu$Year
    prev_year_snu$Year <- NULL
    prev_year_snu$time <- paste(prev_year_snu$time, 99, sep = "")
    result <- rbind(prev_year_snu, prev_Time_snu)
    result <- result[,c("snu1", "time", "HIVraw", "HIVprev", "HIVcov")]
    return(result)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "FALSE" & byyear == "FALSE"){
    prev_snu <- ddply(data, "snu1", HIVprevs)
    prev_snu$HIVraw <- prev_snu$V1
    prev_snu$HIVprev <- prev_snu$V2
    prev_snu$HIVcov <- prev_snu$V3
    prev_snu$V1 <- prev_snu$V2 <- prev_snu$V3 <- NULL
    return(prev_snu)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "TRUE" & byyear == "FALSE"){
    prev_Time <- ddply(data, "time", HIVprevs)
    prev_Time$HIVraw <- prev_Time$V1
    prev_Time$HIVprev <- prev_Time$V2
    prev_Time$HIVcov <- prev_Time$V3
    prev_Time$snu1 <- "All"
    prev_Time$V1 <- prev_Time$V2 <- prev_Time$V3 <- NULL
    prev_Time <- prev_Time[,c("snu1", "time", "HIVraw", "HIVprev", "HIVcov")]
    return(prev_Time)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "FALSE" & byyear == "TRUE"){
    prev_year <- ddply(data, "Year", HIVprevs)
    prev_year$HIVraw <- prev_year$V1
    prev_year$HIVprev <- prev_year$V2
    prev_year$HIVcov <- prev_year$V3
    prev_year$snu1 <- "All"
    prev_year$time <- prev_year$Year
    prev_year$V1 <- prev_year$V2 <- prev_year$V3 <- prev_year$Year <- NULL
    prev_year <- prev_year[,c("snu1", "time", "HIVraw", "HIVprev", "HIVcov")]
    return(prev_year)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "TRUE" & byyear == "FALSE"){
    prev_Time_snu <- ddply(data, c("snu1", "time"), HIVprevs)
    prev_Time_snu$HIVraw <- prev_Time_snu$V1
    prev_Time_snu$HIVprev <- prev_Time_snu$V2
    prev_Time_snu$HIVcov <- prev_Time_snu$V3
    prev_Time_snu$V1 <- prev_Time_snu$V2 <- prev_Time_snu$V3 <- NULL
    prev_Time_snu <- prev_Time_snu[,c("snu1", "time", "HIVraw", "HIVprev", "HIVcov")]
    return(prev_Time_snu)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "FALSE" & byyear == "TRUE"){
    prev_year_snu <- ddply(data, c("snu1", "Year"), HIVprevs)
    prev_year_snu$HIVraw <- prev_year_snu$V1
    prev_year_snu$HIVprev <- prev_year_snu$V2
    prev_year_snu$HIVcov <- prev_year_snu$V3
    prev_year_snu$time <- prev_year_snu$Year
    prev_year_snu$V1 <- prev_year_snu$V2 <- prev_year_snu$V3 <- prev_year_snu$Year <- NULL
    prev_year_snu <- prev_year_snu[,c("snu1", "time", "HIVraw", "HIVprev", "HIVcov")]
    return(prev_year_snu)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "TRUE" & byyear == "TRUE"){
    prev_year <- ddply(data, "Year", HIVprevs)
    prev_year$HIVraw <- prev_year$V1
    prev_year$HIVprev <- prev_year$V2
    prev_year$HIVcov <- prev_year$V3
    prev_year$V1 <- prev_year$V2 <- prev_year$V3 <- NULL
    
    prev_Time <- ddply(data, "time", HIVprevs)
    prev_Time$HIVraw <- prev_Time$V1
    prev_Time$HIVprev <- prev_Time$V2
    prev_Time$HIVcov <- prev_Time$V3
    prev_Time$V1 <- prev_Time$V2 <- prev_Time$V3 <- NULL
    
    prev_year$time <- prev_year$Year
    prev_year$Year <- NULL
    prev_year$time <- paste(prev_year$time, 99, sep = "")
    result2 <- rbind(prev_year, prev_Time)
    result2$snu1 <- "All"
    result2 <- result2[,c("snu1", "time", "HIVraw", "HIVprev", "HIVcov")]
    return(result2)
  }
}