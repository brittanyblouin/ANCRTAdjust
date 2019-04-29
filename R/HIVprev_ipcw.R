#' Adjustment for missing reporting periods
#'
#' Adjusts HIV prevalence from ANC-RT data for missing data due to facilities missing reporting periods
#'
#' This function has been developed to adjust HIV prevalence from ANC-RT data for missing reporting periods within a facility.
#' Inverse probability of censoring weighting is used to account for the missing reporting periods.  The weights are 
#' calculated conditional on the facility.  The adjustment is only made for missing reporting periods in between the first and last 
#' reported period in a facility to avoid adjusting for periods where the facility may have been inoperational. The adjusted HIV 
#' prevalence can be reported stratified by reporting period, year and/or subnational unit 1, according to user inputs.
#'
#' @param Data The ANC-RT dataset.  Ideally the functions \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} have been run on the data to properly
#' prepare the data for use here.  The dataset must have the following variables:
#'  \itemize{
#'   \item \code{faciluid}: Facility ID.
#'   \item \code{snu1}: The subnational unit 1.
#'   \item \code{Time}: The time period that the data was collected (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{Year}: The year that the data was collected.
#'   \item \code{n_clients}: The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'   \item \code{n_status}:  The number of women from the specified facility, during the specified time period, that had their HIV status ascertained at their first ANC visit, either by testing or through previous knowledge.
#'   \item \code{testpos}: The number of women from the specified facility, during the specified time period, that tested positive for HIV at their first ANC visit.
#'   \item \code{knownpos}: The number of women from the specified facility, during the specified time period, that already knew that they were HIV-positive at their first ANC visit.
#'   \item \code{testneg}: The number of women from the specified facility, during the specified time period, that tested negative for HIV at their first ANC visit.
#'   \item \code{TotPos}: Total number of positive HIV cases (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{n_stat}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{Prv}: The HIV prevalence at the specified facility, during the specified time period (generated using the \link[ANCRTAdjust]{mt_adjust} function).
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
#'   \item{HIVprev}: The HIV prevalence adjusted for missing reporting periods and all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'  }
#'
#' @export
#' 

HIVprev_ipcw <- function(Data, byperiod = "FALSE", bysnu1 = "FALSE", byyear = "FALSE") {

  Data$NoData <- ifelse(is.na(Data$n_clients) & (is.na(Data$n_status) | Data$n_status == 0) & (is.na(Data$testpos) | Data$testpos == 0) 
                      & (is.na(Data$testneg) | Data$testneg == 0) & (is.na(Data$knownpos) | Data$knownpos == 0), 1, 0)
  Data <- Data[Data$NoData == 0]
  Data$Ones <- 1
  Data$ID <- recode.cluster(Data$faciluid)
  quarters <- data.frame(ID = Data$ID, Time = Data$Time, Ones = Data$Ones)
  wide <- reshape(quarters, idvar = "ID", timevar = "time", direction = "wide")
  wide[is.na(wide)] <- 0

  long <- reshape(wide, idvar = "ID", timevar = "time", direction = "long")
  names(long)[3]<-"uncensored"

  Data2 <- merge(Data, long, by = c("ID", "time"), all = TRUE)

  Data3 <- NULL
  for (i in 1:length(unique(Data2$ID))) {
    temp <- Data2[Data2$ID == i]
    temp <- temp[order(Time), ]
    temp$sum <- cumsum(temp$uncensored)
    temp$first <- ifelse(temp$sum == 1 & temp$uncensored == 1, 1, 0)
    temp$last <- ifelse(temp$sum == max(temp$sum) & temp$uncensored == 1, 1, 0)
    Time_first <- temp$Time[temp$first == 1]
    Time_last <- temp$Time[temp$last == 1]
    temp$include <- ifelse((temp$Time >= Time_first) & (temp$Time <= Time_last), 1, 0)
    temp <- temp[temp$include == 1, ]
    temp$include <- temp$first <- temp$last <- temp$sum <- NULL
    Data3 <- rbind(Data3, temp)
  }
    
  prob.cens <- function(data) {
    weight <- sum(data$uncensored) / length(data$uncensored)
    return(weight)
  }
  weights <- ddply(Data3, c("ID"), prob.cens)
  Data3 <- merge(Data3, weights, by = "ID", all = TRUE)
  Data3$weight <- 1/Data3$V1
  Data4 <- Data3[Data3$uncensored == 1,]
  Data4$Ones <- Data4$NoData <- Data4$uncensored <- Data4$V1 <- NULL
  
  Data4$weight_clients <- Data4$n_stat * Data4$weight
  HIV_prev <- round(((weighted.mean(Data4$Prv, w = Data4$weight_clients, na.rm = TRUE)) * 100), 2)
  
  HIVprevs <- function(data){
    prev <- round(((weighted.mean((data$TotPos) / data$n_stat, w = data$weight_clients, na.rm = TRUE)) * 100), 2)
    return(prev)
  }
  prev_Time_snu <- ddply(Data4, c("snu1", "time"), HIVprevs)
  prev_Time_snu$HIVprev <- prev_Time_snu$V1
  prev_Time_snu$V1 <- NULL
  
  prev_snu <- ddply(Data4, "snu1", HIVprevs)
  prev_snu$HIVprev <- prev_snu$V1
  prev_snu$V1 <- NULL
  
  prev_Time <- ddply(Data4, "time", HIVprevs)
  prev_Time$HIVprev <- prev_Time$V1
  prev_Time$V1 <- NULL
  
  prev_year_snu <- ddply(Data4, c("snu1", "Year"), HIVprevs)
  prev_year_snu$HIVprev <- prev_year_snu$V1
  prev_year_snu$V1 <- NULL
  
  prev_year <- ddply(Data4, "Year", HIVprevs)
  prev_year$HIVprev <- prev_year$V1
  prev_year$V1 <- NULL
  
  if (bysnu1 == "FALSE" & byperiod == "FALSE" & byyear == "FALSE"){
    return(HIV_prev)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "TRUE" & byyear == "TRUE"){
    prev_year_snu$Time <- prev_year_snu$Year
    prev_year_snu$Year <- NULL
    prev_year_snu$Time <- paste(prev_year_snu$Time, 99, sep = "")
    result <- rbind(prev_year_snu, prev_Time_snu)
    return(result)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "FALSE" & byyear == "FALSE") {
    return(prev_snu)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "TRUE" & byyear == "FALSE") {
    return(prev_Time)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "FALSE" & byyear == "TRUE") {
    return(prev_year)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "TRUE" & byyear == "FALSE") {
    return(prev_Time_snu)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "FALSE" & byyear == "TRUE") {
    return(prev_year_snu)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "TRUE" & byyear == "TRUE") {
    prev_year$Time <- prev_year$Year
    prev_year$Year <- NULL
    prev_year$Time <- paste(prev_year$Time, 99, sep = "")
    result2 <- rbind(prev_year, prev_Time)
    return(result2)
  }
}