#' Adjustment for missing reporting periods
#'
#' Adjusts HIV prevalence and HIV testing coverage from ANC-RT data for missing data due to facilities missing reporting periods
#'
#' This function has been developed to adjust HIV prevalence and HIV testing coverage from ANC-RT data for missing reporting periods within a facility.
#' Inverse probability of censoring weighting is used to account for the missing reporting periods.  The weights are 
#' calculated conditional on the facility.  The adjustment is only made for missing reporting periods in between the first and last 
#' reported period in a facility to avoid adjusting for periods where the facility may have been inoperational. The adjusted HIV 
#' prevalence and HIV testing coverage can be reported stratified by reporting period, year and/or subnational unit 1, according to user inputs.
#'
#' @param data The ANC-RT dataset.  The functions \link[ANCRTAdjust]{name_var}, \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} should have been run on the data to properly
#' prepare the data for use here.  The dataset must have the following variables:
#'  \itemize{
#'   \item \code{faciluid}: Facility ID.
#'   \item \code{time}: The time period that the data was collected.
#'   \item \code{n_clients}: The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'   \item \code{n_status}:  The number of women from the specified facility, during the specified time period, that had their HIV status ascertained at their first ANC visit, either by testing or through previous knowledge.
#'   \item \code{testpos}: The number of women from the specified facility, during the specified time period, that tested positive for HIV at their first ANC visit.
#'   \item \code{knownpos}: The number of women from the specified facility, during the specified time period, that already knew that they were HIV-positive at their first ANC visit.
#'   \item \code{testneg}: The number of women from the specified facility, during the specified time period, that tested negative for HIV at their first ANC visit.
#'   \item \code{totpos}: Total number of positive HIV cases.
#'   \item \code{totpos_c}: Cleaned \code{totpos} (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{n_status_c}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{snu1}: The subnational unit 1 (only required if results are to be stratified by snu1).
#'   \item \code{Year}: The year that the data was collected (only required if results are to be stratified by year).
#'       }
#' @param bysnu1 "TRUE" indicates that the results be stratified by snu1.  "FALSE" indicates that the results not be stratified.
#' @param byperiod "TRUE" indicates that the results be stratified by reporting period.  "FALSE" indicates that the results not be stratified.
#' @param byyear "TRUE" indicates that the results be stratified by year.  "FALSE" indicates that the results not be stratified.
#'
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A table (in dataframe format) indicating the results, stratified according to user inputs.  The results table includes the following columns:
#'  \itemize{
#'   \item \code{snu1} (only if results were stratified by snu1): The subnational unit 1.  
#'   \item \code{Time} (only if results were stratified by period and/or year): The time period.  When results are stratified by year and reporting period, the digits "99" are suffixed to the year 
#'   (i.e. "201599" refers to the yearly result for 2015).
#'   \item \code{HIVprev}: The HIV prevalence adjusted for missing reporting periods and all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'   \item \code{HIVcov}: The HIV testing coverage adjusted for missing reporting periods and all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'  }
#'
#' @export
#' 

HIVprev_ipcw <- function(data, byperiod = "FALSE", bysnu1 = "FALSE", byyear = "FALSE") {

  data$NoData <- ifelse(is.na(data$n_clients) & (is.na(data$n_status) | data$n_status == 0) & (is.na(data$testpos) | data$testpos == 0) 
                      & (is.na(data$testneg) | data$testneg == 0) & (is.na(data$knownpos) | data$knownpos == 0) & 
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
    Time_first <- temp$time[temp$first == 1]
    Time_last <- temp$time[temp$last == 1]
    temp$include <- ifelse((temp$time >= Time_first) & (temp$time <= Time_last), 1, 0)
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
  
  HIVprevs <- function(data){
    raw <- round(((weighted.mean((data$totpos) / data$n_status, w = data$n_status, na.rm = TRUE)) * 100), 2)
    prev <- round(((weighted.mean((data$totpos_c) / data$n_status_c, w = data$weight_clients, na.rm = TRUE)) * 100), 2)
    cov <- round(((weighted.mean((data$n_status_c) / data$n_clients, w = data$weight_cov, na.rm = TRUE)) * 100), 2)
    return(c(raw, prev, cov))
  }
  
  if (bysnu1 == "FALSE" & byperiod == "FALSE" & byyear == "FALSE"){
    HIVraw <- round(((weighted.mean((data4$totpos) / data4$n_status, w = data4$n_status, na.rm = TRUE)) * 100), 2)
    HIVprev <- round(((weighted.mean((data4$totpos_c) / data4$n_status_c, w = data4$weight_clients, na.rm = TRUE)) * 100), 2)
    HIVcov <- round(((weighted.mean((data4$n_status_c) / data4$n_clients, w = data4$weight_cov, na.rm = TRUE)) * 100), 2)
    snu1 <- "All"
    return(data.frame(snu1, HIVraw, HIVprev, HIVcov))
  }
  
  if (bysnu1 == "TRUE" & byperiod == "TRUE" & byyear == "TRUE"){
    prev_year_snu <- ddply(data4, c("snu1", "Year"), HIVprevs)
    prev_year_snu$HIVraw <- prev_year_snu$V1
    prev_year_snu$HIVprev <- prev_year_snu$V2
    prev_year_snu$HIVcov <- prev_year_snu$V3
    prev_year_snu$V1 <- prev_year_snu$V2 <- prev_year_snu$V3 <- NULL
    
    prev_Time_snu <- ddply(data4, c("snu1", "time"), HIVprevs)
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
    prev_snu <- ddply(data4, "snu1", HIVprevs)
    prev_snu$HIVraw <- prev_snu$V1
    prev_snu$HIVprev <- prev_snu$V2
    prev_snu$HIVcov <- prev_snu$V3
    prev_snu$V1 <- prev_snu$V2 <- prev_snu$V3 <- NULL
    return(prev_snu)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "TRUE" & byyear == "FALSE"){
    prev_Time <- ddply(data4, "time", HIVprevs)
    prev_Time$HIVraw <- prev_Time$V1
    prev_Time$HIVprev <- prev_Time$V2
    prev_Time$HIVcov <- prev_Time$V3
    prev_Time$snu1 <- "All"
    prev_Time$V1 <- prev_Time$V2 <- prev_Time$V3 <- NULL
    prev_Time <- prev_Time[,c("snu1", "time", "HIVraw", "HIVprev", "HIVcov")]
    return(prev_Time)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "FALSE" & byyear == "TRUE"){
    prev_year <- ddply(data4, "Year", HIVprevs)
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
    prev_Time_snu <- ddply(data4, c("snu1", "time"), HIVprevs)
    prev_Time_snu$HIVraw <- prev_Time_snu$V1
    prev_Time_snu$HIVprev <- prev_Time_snu$V2
    prev_Time_snu$HIVcov <- prev_Time_snu$V3
    prev_Time_snu$V1 <- prev_Time_snu$V2 <- prev_Time_snu$V3 <- NULL
    prev_Time_snu <- prev_Time_snu[,c("snu1", "time", "HIVraw", "HIVprev", "HIVcov")]
    return(prev_Time_snu)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "FALSE" & byyear == "TRUE"){
    prev_year_snu <- ddply(data4, c("snu1", "Year"), HIVprevs)
    prev_year_snu$HIVraw <- prev_year_snu$V1
    prev_year_snu$HIVprev <- prev_year_snu$V2
    prev_year_snu$HIVcov <- prev_year_snu$V3
    prev_year_snu$time <- prev_year_snu$Year
    prev_year_snu$V1 <- prev_year_snu$V2 <- prev_year_snu$V3 <- prev_year_snu$Year <- NULL
    prev_year_snu <- prev_year_snu[,c("snu1", "time", "HIVraw", "HIVprev", "HIVcov")]
    return(prev_year_snu)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "TRUE" & byyear == "TRUE"){
    prev_year <- ddply(data4, "Year", HIVprevs)
    prev_year$HIVraw <- prev_year$V1
    prev_year$HIVprev <- prev_year$V2
    prev_year$HIVcov <- prev_year$V3
    prev_year$V1 <- prev_year$V2 <- prev_year$V3 <- NULL
    
    prev_Time <- ddply(data4, "time", HIVprevs)
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