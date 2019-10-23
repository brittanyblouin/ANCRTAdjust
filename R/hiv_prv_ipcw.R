#' Adjustment for missing reporting periods
#'
#' Adjusts HIV prevalence and HIV testing coverage from ANC-RT data for missing data due to facilities missing reporting periods. 
#' This function has been developed to adjust HIV prevalence and HIV testing coverage from ANC-RT data for missing reporting periods within a facility.
#' Inverse probability of censoring weighting is used to account for the missing reporting periods. The weights are 
#' calculated conditional on the facility. The adjustment is only made for missing reporting periods in between the first and last 
#' reported period in a facility to avoid adjusting for periods when the facility may have been inoperational. Specifically, the weights are calculated as followed:
#' \deqn{ w = \frac{1}{P(\text{Not being censored } | \text{ facility})}}
#' The adjusted HIV prevalence and HIV testing coverage can be reported stratified by reporting period, year and/or subnational unit 1, according to user inputs.
#'
#' @param data The ANC-RT dataset.  The functions \link[ANCRTAdjust]{name_var}, \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} should have been run on the data to properly
#' prepare the data for use here.  The dataset must have the following variables:
#'  \itemize{
#'   \item \code{faciluid}: Facility ID.
#'   \item \code{time}: The time period over which the data was collected.
#'   \item \code{n_clients}: The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'   \item \code{n_status}:  The number of women from the specified facility, during the specified time period, that had their HIV status ascertained at their first ANC visit, either by testing or through previous knowledge.
#'   \item \code{testpos}: The number of women from the specified facility, during the specified time period, that tested positive for HIV at their first ANC visit.
#'   \item \code{knownpos}: The number of women from the specified facility, during the specified time period, that already knew that they were HIV-positive at their first ANC visit.
#'   \item \code{testneg}: The number of women from the specified facility, during the specified time period, that tested negative for HIV at their first ANC visit.
#'   \item \code{totpos}: Total number of positive HIV cases.
#'   \item \code{totpos_c}: Cleaned \code{totpos} (generated using the \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} functions).
#'   \item \code{n_status_c}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} functions).
#'   \item \code{snu1}: The subnational unit 1 (only required if results are to be stratified by snu1).
#'   \item \code{Year}: The year that the data was collected (only required if results are to be stratified by year).
#'       }
#' @param by_snu1 TRUE indicates that the results be stratified by snu1.  FALSE indicates that the results not be stratified.
#' @param by_period TRUE indicates that the results be stratified by reporting period.  FALSE indicates that the results not be stratified.
#' @param by_year TRUE indicates that the results be stratified by year.  FALSE indicates that the results not be stratified.
#'
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A table (in dataframe format) indicating the results, stratified according to user inputs.  The results table includes the following columns:
#'  \itemize{
#'   \item \code{snu1} (only if results were stratified by snu1): The subnational unit 1.  
#'   \item \code{time} (only if results were stratified by period and/or year): The time period.  When results are stratified by year and reporting period, the digits "99" are suffixed to the year 
#'   (i.e. "201599" refers to the yearly result for 2015).
#'   \item \code{hiv_prv}: The HIV prevalence adjusted for missing reporting periods and all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'   \item \code{hiv_cov}: The HIV testing coverage adjusted for missing reporting periods and all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'  }
#'
#' @export
#' 

hiv_prv_ipcw <- function(data, by_period = FALSE, by_snu1 = FALSE, by_year = FALSE) {

  data$NoData <- ifelse(is.na(data$n_clients) & (is.na(data$n_status) | data$n_status == 0) & (is.na(data$testpos) | data$testpos == 0) &
                        (is.na(data$testneg) | data$testneg == 0) & (is.na(data$knownpos) | data$knownpos == 0) & 
                        (is.na(data$totpos) | data$totpos == 0), 1, 0)
  data <- data[data$NoData == 0,]
  data$Ones <- 1
  quarters <- data.frame(faciluid = data$faciluid, time = data$time, Ones = data$Ones)
  wide <- reshape(quarters, idvar = "faciluid", timevar = "time", direction = "wide")
  wide[is.na(wide)] <- 0

  long <- reshape(wide, idvar = "faciluid", timevar = "time", direction = "long")
  names(long)[3]<-"uncensored"

  data2 <- merge(data, long, by = c("faciluid", "time"), all = TRUE)
  data2$faciluid <- as.factor(data2$faciluid)

  data3 <- NULL
  for (i in levels(data2$faciluid)) {
    temp <- data2[data2$faciluid == i,]
    temp <- temp[order(temp$time), ]
    temp$sum <- cumsum(temp$uncensored)
    temp$first <- ifelse(temp$sum == 1 & temp$uncensored == 1, 1, 0)
    temp$last <- ifelse(temp$sum == max(temp$sum) & temp$uncensored == 1, 1, 0)
    time_first <- temp$time[temp$first == 1]
    time_last <- temp$time[temp$last == 1]
    temp$include <- ifelse((temp$time >= time_first) & (temp$time <= time_last), 1, 0)
    temp <- temp[temp$include == 1, ]
    temp$include <- temp$first <- temp$last <- temp$sum <- NULL
    data3 <- rbind(data3, temp)
  }
    
  prob.cens <- function(data) {
    weight <- sum(data$uncensored) / length(data$uncensored)
    return(weight)
  }
  weights <- ddply(data3, c("faciluid"), prob.cens)
  data3 <- merge(data3, weights, by = "faciluid", all = TRUE)
  data3$weight <- 1/data3$V1
  data4 <- data3[data3$uncensored == 1,]
  data4$Ones <- data4$NoData <- data4$uncensored <- data4$V1 <- NULL
  
  data4$weight_clients <- data4$n_status_c * data4$weight
  data4$weight_cov <- data4$n_clients * data4$weight
  
  hiv_prvs <- function(data){
    raw <- round(((weighted.mean((data$totpos) / data$n_status, w = data$n_status, na.rm = TRUE)) * 100), 2)
    prev <- round(((weighted.mean((data$totpos_c) / data$n_status_c, w = data$weight_clients, na.rm = TRUE)) * 100), 2)
    cov <- round(((weighted.mean((data$n_status_c) / data$n_clients, w = data$weight_cov, na.rm = TRUE)) * 100), 2)
    return(c(raw, prev, cov))
  }
  
  if (by_snu1 == FALSE & by_period == FALSE & by_year == FALSE){
    hiv_raw <- round(((weighted.mean((data4$totpos) / data4$n_status, w = data4$n_status, na.rm = TRUE)) * 100), 2)
    hiv_prv <- round(((weighted.mean((data4$totpos_c) / data4$n_status_c, w = data4$weight_clients, na.rm = TRUE)) * 100), 2)
    hiv_cov <- round(((weighted.mean((data4$n_status_c) / data4$n_clients, w = data4$weight_cov, na.rm = TRUE)) * 100), 2)
    snu1 <- "all"
    return(data.frame(snu1, hiv_raw, hiv_prv, hiv_cov))
  }
  
  if (by_snu1 == TRUE & by_period == TRUE & by_year == TRUE){
    prv_year_snu <- ddply(data4, c("snu1", "Year"), hiv_prvs)
    prv_year_snu$hiv_raw <- prv_year_snu$V1
    prv_year_snu$hiv_prv <- prv_year_snu$V2
    prv_year_snu$hiv_cov <- prv_year_snu$V3
    prv_year_snu$V1 <- prv_year_snu$V2 <- prv_year_snu$V3 <- NULL
    
    prv_time_snu <- ddply(data4, c("snu1", "time"), hiv_prvs)
    prv_time_snu$hiv_raw <- prv_time_snu$V1
    prv_time_snu$hiv_prv <- prv_time_snu$V2
    prv_time_snu$hiv_cov <- prv_time_snu$V3
    prv_time_snu$V1 <- prv_time_snu$V2 <- prv_time_snu$V3 <- NULL
    
    prv_year_snu$time <- prv_year_snu$year
    prv_year_snu$year <- NULL
    prv_year_snu$time <- paste(prv_year_snu$time, 99, sep = "")
    result <- rbind(prv_year_snu, prv_time_snu)
    result <- result[,c("snu1", "time", "hiv_raw", "hiv_prv", "hiv_cov")]
    return(result)
  }
  
  if (by_snu1 == TRUE & by_period == FALSE & by_year == FALSE){
    prv_snu <- ddply(data4, "snu1", hiv_prvs)
    prv_snu$hiv_raw <- prv_snu$V1
    prv_snu$hiv_prv <- prv_snu$V2
    prv_snu$hiv_cov <- prv_snu$V3
    prv_snu$V1 <- prv_snu$V2 <- prv_snu$V3 <- NULL
    return(prv_snu)
  }
  
  if (by_snu1 == FALSE & by_period == TRUE & by_year == FALSE){
    prv_time <- ddply(data4, "time", hiv_prvs)
    prv_time$hiv_raw <- prv_time$V1
    prv_time$hiv_prv <- prv_time$V2
    prv_time$hiv_cov <- prv_time$V3
    prv_time$snu1 <- "all"
    prv_time$V1 <- prv_time$V2 <- prv_time$V3 <- NULL
    prv_time <- prv_time[,c("snu1", "time", "hiv_raw", "hiv_prv", "hiv_cov")]
    return(prv_time)
  }
  
  if (by_snu1 == FALSE & by_period == FALSE & by_year == TRUE){
    prv_year <- ddply(data4, "year", hiv_prvs)
    prv_year$hiv_raw <- prv_year$V1
    prv_year$hiv_prv <- prv_year$V2
    prv_year$hiv_cov <- prv_year$V3
    prv_year$snu1 <- "All"
    prv_year$time <- prv_year$year
    prv_year$V1 <- prv_year$V2 <- prv_year$V3 <- prv_year$year <- NULL
    prv_year <- prv_year[,c("snu1", "time", "hiv_raw", "hiv_prv", "hiv_cov")]
    return(prv_year)
  }
  
  if (by_snu1 == TRUE & by_period == TRUE & by_year == FALSE){
    prv_time_snu <- ddply(data4, c("snu1", "time"), hiv_prvs)
    prv_time_snu$hiv_raw <- prv_time_snu$V1
    prv_time_snu$hiv_prv <- prv_time_snu$V2
    prv_time_snu$hiv_cov <- prv_time_snu$V3
    prv_time_snu$V1 <- prv_time_snu$V2 <- prv_time_snu$V3 <- NULL
    prv_time_snu <- prv_time_snu[,c("snu1", "time", "hiv_raw", "hiv_prv", "hiv_cov")]
    return(prv_time_snu)
  }
  
  if (by_snu1 == TRUE & by_period == FALSE & by_year == TRUE){
    prv_year_snu <- ddply(data4, c("snu1", "year"), hiv_prvs)
    prv_year_snu$hiv_raw <- prv_year_snu$V1
    prv_year_snu$hiv_prv <- prv_year_snu$V2
    prv_year_snu$hiv_cov <- prv_year_snu$V3
    prv_year_snu$time <- prv_year_snu$year
    prv_year_snu$V1 <- prv_year_snu$V2 <- prv_year_snu$V3 <- prv_year_snu$year <- NULL
    prv_year_snu <- prv_year_snu[,c("snu1", "time", "hiv_raw", "hiv_prv", "hiv_cov")]
    return(prv_year_snu)
  }
  
  if (by_snu1 == FALSE & by_period == TRUE & by_year == TRUE){
    prv_year <- ddply(data4, "year", hiv_prvs)
    prv_year$hiv_raw <- prv_year$V1
    prv_year$hiv_prv <- prv_year$V2
    prv_year$hiv_cov <- prv_year$V3
    prv_year$V1 <- prv_year$V2 <- prv_year$V3 <- NULL
    
    prv_time <- ddply(data4, "time", hiv_prvs)
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