#' Calculate HIV prevalence and HIV testing coverage
#'
#' Calculates HIV prevalence and HIV testing coverage stratified by snu1, year and/or reporting period.
#'
#' This function has been developed to calculate HIV prevalence and HIV testing coverage from ANC-RT data.
#' The HIV prevalence and testing coverage can be reported stratified by reporting period, year and/or subnational unit 1, 
#' according to user inputs.
#'
#' @param data The ANC-RT dataset. The functions \link[ANCRTAdjust]{check_data}, \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} should have been run on the data to properly
#' prepare the data for use here. The dataset must have the following variables:
#'  \itemize{
#'   \item \code{n_clients}: The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'   \item \code{totpos_c}: Cleaned \code{totpos} (generated using the \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} functions).
#'   \item \code{n_status_c}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} functions).
#'   \item \code{snu1}: The subnational unit 1 (only required if results are to be stratified by snu1).
#'   \item \code{time}: The time period over which the data was collected (only required if results are to be stratified by the reporting time period).
#'   \item \code{year}: The year that the data was collected (only required if results are to be stratified by year).
#'     }
#' @param by_snu1 TRUE indicates that the results be stratified by snu1.  FALSE" indicates that the results not be stratified.
#' @param by_period TRUE indicates that the results be stratified by reporting period.  FALSE" indicates that the results not be stratified.
#' @param by_year TRUE indicates that the results be stratified by year.  FALSE" indicates that the results not be stratified.
#'
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A table (in dataframe format) indicating the results, stratified according to user inputs.  The results table includes the following columns:
#'  \itemize{
#'   \item{snu1} (only if results were stratified by snu1): The subnational unit 1.  
#'   \item{time} (only if results were stratified by time period and/or year): The time frame.  When results are stratified by year and reporting period, the digits "99" are suffixed to the year 
#'   (i.e. "201599" refers to the yearly result for 2015).
#'   \item{hiv_raw}: The raw HIV prevalence (i.e. with no previous adjustments).
#'   \item{hiv_prv}: The HIV prevalence adjusted for all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'   \item{hiv_cov}: The HIV testing coverage adjusted for all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'  }
#'
#' @export
#' 

hiv_prv_cov <- function(data, by_period = FALSE, by_snu1 = FALSE, by_year = FALSE) {
    
  hiv_prvs <- function(data) {
    raw <- round(((weighted.mean((data$totpos) / data$n_status, w = data$n_status, na.rm = TRUE)) * 100), 2)
    prv <- round(((weighted.mean((data$totpos_c) / data$n_status_c, w = data$n_status_c, na.rm = TRUE)) * 100), 2)
    cov <- round(((weighted.mean((data$n_status_c) / data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100), 2)
    return(c(raw, prv, cov))
  }
  
  if (by_snu1 == FALSE & by_period == FALSE & by_year == FALSE) {
    hiv_raw <- round(((weighted.mean((data$totpos) / data$n_status, w = data$n_status, na.rm = TRUE)) * 100), 2)
    hiv_prv <- round(((weighted.mean((data$totpos_c) / data$n_status_c, w = data$n_status_c, na.rm = TRUE)) * 100), 2)
    hiv_cov <- round(((weighted.mean((data$n_status_c) / data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100), 2)
    snu1 <- "all"
    return(data.frame(snu1, hiv_raw, hiv_prv, hiv_cov))
  }
  
  if (by_snu1 == TRUE & by_period == TRUE & by_year == TRUE){
    prv_year_snu <- ddply(data, c("snu1", "year"), hiv_prvs)
    prv_year_snu$hiv_raw <- prv_year_snu$V1
    prv_year_snu$hiv_prv <- prv_year_snu$V2
    prv_year_snu$hiv_cov <- prv_year_snu$V3
    prv_year_snu$V1 <- prv_year_snu$V2 <- prv_year_snu$V3 <- NULL
    
    prv_time_snu <- ddply(data, c("snu1", "time"), hiv_prvs)
    prv_time_snu$hiv_raw <- prv_time_snu$V1
    prv_time_snu$hiv_prv <- prv_time_snu$V2
    prv_time_snu$hiv_cov <- prv_time_snu$V3
    prv_time_snu$V1 <- prv_time_snu$V2 <- prv_time_snu$V3 <- NULL
    
    prv_year_snu$time <- prv_year_snu$year
    prv_year_snu$year <- NULL
    prv_year_snu$time <- paste(prv_year_snu$time, 99, sep = "")
    result <- rbind(prv_year_snu, prv_time_snu)
    result <- result[, c("snu1", "time", "hiv_raw", "hiv_prv", "hiv_cov")]
    return(result)
  }
  
  if (by_snu1 == TRUE & by_period == FALSE & by_year == FALSE){
    prv_snu <- ddply(data, "snu1", hiv_prvs)
    prv_snu$hiv_raw <- prv_snu$V1
    prv_snu$hiv_prv <- prv_snu$V2
    prv_snu$hiv_cov <- prv_snu$V3
    prv_snu$V1 <- prv_snu$V2 <- prv_snu$V3 <- NULL
    return(prv_snu)
  }
  
  if (by_snu1 == FALSE & by_period == TRUE & by_year == FALSE){
    prv_time <- ddply(data, "time", hiv_prvs)
    prv_time$hiv_raw <- prv_time$V1
    prv_time$hiv_prv <- prv_time$V2
    prv_time$hiv_cov <- prv_time$V3
    prv_time$snu1 <- "all"
    prv_time$V1 <- prv_time$V2 <- prv_time$V3 <- NULL
    prv_time <- prv_time[, c("snu1", "time", "hiv_raw", "hiv_prv", "hiv_cov")]
    return(prv_time)
  }
  
  if (by_snu1 == FALSE & by_period == FALSE & by_year == TRUE){
    prv_year <- ddply(data, "year", hiv_prvs)
    prv_year$hiv_raw <- prv_year$V1
    prv_year$hiv_prv <- prv_year$V2
    prv_year$hiv_cov <- prv_year$V3
    prv_year$snu1 <- "all"
    prv_year$time <- prv_year$year
    prv_year$V1 <- prv_year$V2 <- prv_year$V3 <- prv_year$year <- NULL
    prv_year <- prv_year[, c("snu1", "time", "hiv_raw", "hiv_prv", "hiv_cov")]
    return(prv_year)
  }
  
  if (by_snu1 == TRUE & by_period == TRUE & by_year == FALSE){
    prv_time_snu <- ddply(data, c("snu1", "time"), hiv_prvs)
    prv_time_snu$hiv_raw <- prv_time_snu$V1
    prv_time_snu$hiv_prv <- prv_time_snu$V2
    prv_time_snu$hiv_cov <- prv_time_snu$V3
    prv_time_snu$V1 <- prv_time_snu$V2 <- prv_time_snu$V3 <- NULL
    prv_time_snu <- prv_time_snu[, c("snu1", "time", "hiv_raw", "hiv_prv", "hiv_cov")]
    return(prv_time_snu)
  }
  
  if (by_snu1 == TRUE & by_period == FALSE & by_year == TRUE){
    prv_year_snu <- ddply(data, c("snu1", "year"), hiv_prvs)
    prv_year_snu$hiv_raw <- prv_year_snu$V1
    prv_year_snu$hiv_prv <- prv_year_snu$V2
    prv_year_snu$hiv_cov <- prv_year_snu$V3
    prv_year_snu$time <- prv_year_snu$year
    prv_year_snu$V1 <- prv_year_snu$V2 <- prv_year_snu$V3 <- prv_year_snu$year <- NULL
    prv_year_snu <- prv_year_snu[,c("snu1", "time", "hiv_raw", "hiv_prv", "hiv_cov")]
    return(prv_year_snu)
  }
  
  if (by_snu1 == FALSE & by_period == TRUE & by_year == TRUE){
    prv_year <- ddply(data, "year", hiv_prvs)
    prv_year$hiv_raw <- prv_year$V1
    prv_year$hiv_prv <- prv_year$V2
    prv_year$hiv_cov <- prv_year$V3
    prv_year$V1 <- prv_year$V2 <- prv_year$V3 <- NULL
    
    prv_time <- ddply(data, "time", hiv_prvs)
    prv_time$hiv_raw <- prv_time$V1
    prv_time$hiv_prv <- prv_time$V2
    prv_time$hiv_cov <- prv_time$V3
    prv_time$V1 <- prv_time$V2 <- prv_time$V3 <- NULL
    
    prv_year$time <- prv_year$year
    prv_year$year <- NULL
    prv_year$time <- paste(prv_year$time, 99, sep = "")
    result2 <- rbind(prv_year, prv_time)
    result2$snu1 <- "all"
    result2 <- result2[,c("snu1", "time", "hiv_raw", "hiv_prv", "hiv_cov")]
    return(result2)
  }
}