#' Imperfect coverage adjustment
#'
#' Adjusts HIV prevalence from ANC-RT data for imperfect HIV testing coverage
#'
#' This function has been developed to adjust HIV prevalence from ANC-RT data for imperfect (i.e. less than 100\%) HIV testing coverage.
#' The adjustment model is a logisitic regression model that models HIV prevalence conditional on HIV testing coverage (relationship modeled with
#' a natural sline with one knot at 33\%), time (relationship modeled as a fixed effect) and facility (relationship modeled as a fixed effect).  
#' The adjusted HIV prevalence can be reported stratified by reporting period, year and/or subnational unit 1, according to user inputs.
#'
#' @param data The ANC-RT dataset.  Ideally the functions \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} have been run on the data to properly
#' prepare the data for use here.  The dataset must have the following variables:
#'  \itemize{
#'   \item \code{ID}: Facility ID (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{snu1}: The subnational unit 1.
#'   \item \code{Time}: The quarter that the data was collected (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{Year}: The year that the data was collected.
#'   \item \code{testpos}: The number of women from the specified facility, during the specified time period, that tested positive for HIV at their first ANC visit.
#'   \item \code{knownpos}: The number of women from the specified facility, during the specified time period, that already knew that they were HIV-positive at their first ANC visit.
#'   \item \code{n_status}:  The number of women from the specified facility, during the specified time period, that had their HIV status ascertained at their first ANC visit, either by testing or through previous knowledge.
#'   \item \code{TotPos}: Total number of positive HIV cases (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{n_stat}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{n_clients}: The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'  }
#' @param bysnu1 "TRUE" indicates that the results be stratified by snu1.  "FALSE" indicates that the results not be stratified.
#' @param byperiod "TRUE" indicates that the results be stratified by reporting period.  "FALSE" indicates that the results not be stratified.
#' @param byyear "TRUE" indicates that the results be stratified by year.  "FALSE" indicates that the results not be stratified.
#'
#' @import stringr
#'
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A table (in dataframe format) indicating the results, stratified according to user inputs.  The results table includes the following columns:
#'  \itemize{
#'   \item{snu1}: The subnational unit 1.  "All" refers to results not stratified by snu1.
#'   \item{Time}: The time frame.  When results are stratified by year and reporting period, the digits "99" are suffixed to the year (i.e. "201599" refers to the yearly result for 2015).
#'   "9999" refers to the results not stratified by time (i.e. not stratified by period or year).
#'   \item{HIV_fulladjust}: The HIV prevalence adjusted for imperfect testing coverage and all previous adjustments (i.e. data cleaning and adjustment for multiple testing, if performed).
#'   \item{RR_observed}: The relative bias comparing the HIV prevalence at the observed HIV testing coverage to the predicted HIV prevalence when HIV testing coverage is set to 100\%.
#'   \item{RR_90}: The relative bias comparing the predicted HIV prevalence when HIV testing coverage is set to 90\% to the predicted HIV prevalence when HIV testing coverage is set to 100\%.
#'   \item{HIV_raw}: The HIV prevalence using the raw data.
#'   \item{HIV_cleaned}: The HIV prevalence using the cleaned data (and following adjustment for multiple testing, if performed).
#'  }
#'
#' @export
#' 
imperfectcov_adjust <- function(data, byperiod = "FALSE", bysnu1 = "FALSE", byyear = "FALSE"){
  
  HIVprevs <- function(data){
    prev_raw <- round(((weighted.mean((data$testpos + data$knownpos) / data$n_status, w = data$n_status, na.rm = TRUE)) * 100),2)
    prev_adj <- round(((weighted.mean((data$TotPos) / data$n_stat, w = data$n_stat, na.rm = TRUE)) * 100),2)
    HIVprevs <- c(prev_raw, prev_adj)
    return(HIVprevs)
  }
  
  prevalences <- ddply(data, c("snu1", "Time"), HIVprevs)
  prev.snu <- ddply(data, "snu1", HIVprevs)
  prev.snu$Time <- 9999 
  prev.time <- ddply(data, "Time", HIVprevs)
  prev.time$snu1 <- "All"
  prev_raw <- round(((weighted.mean((data$testpos + data$knownpos) / data$n_status, w = data$n_status, na.rm = TRUE)) * 100),2)
  prev_adj <- round(((weighted.mean((data$TotPos) / data$n_stat, w = data$n_stat, na.rm = TRUE)) * 100),2)
  all <- data.frame(snu1 = "All", Time = 9999, V1 = prev_raw, V2 = prev_adj)
  prev.time <- rbind(prev.time, all)
  prev.year <- ddply(data, c("snu1", "Year"), HIVprevs)
  prev.year$Time <- paste(prev.year$Year, 99, sep = "")
  prev.year$Year <- NULL
  prev.year2 <- ddply(data, "Year", HIVprevs)
  prev.year2$snu1 <- "All"
  prev.year2$Time <- paste(prev.year2$Year, 99, sep = "")
  prev.year2$Year <- NULL
  
  prevalences <- rbind(prevalences, prev.snu, prev.time, prev.year, prev.year2)
  prevalences$HIV_raw <- prevalences$V1
  prevalences$HIV_cleaned <- prevalences$V2
  prevalences$V1 <- prevalences$V2 <- NULL
  
  data <- data[!is.na(data$Cov)]
  data <- data[!is.na(data$Prv)]
  
Fit <- glm(cbind(TotPos, n_stat - TotPos) ~ ns(Cov, knots = 1/3) + as.factor(Time) + as.factor(ID) ,
             family = 'binomial', data = data)
  
  data$snu1.Time <- paste(data$snu1, data$Time, sep = "-")
  
  Results_data <- NULL
  SNU1Time.list <- (unique(data$snu1.Time))
  for (i in 1:length(SNU1Time.list)){
    
    OldData <- NewData100 <- NewData90 <- data[data$snu1.Time == SNU1Time.list[i],]
    
    Time <- OldData$Time[1]
    snu1 <- OldData$snu1[1]
    OldData$DenWgt <- OldData$n_clients * OldData$Cov
    PredictObs <- predict(Fit, OldData, type = 'response')
    
    NewData100$Cov <- 1
    NewData100$DenWgt <- NewData100$n_clients * NewData100$Cov
    Predict100 <- predict(Fit, NewData100, type = 'response')
    
    NewData90$Cov <- 0.9
    NewData90$DenWgt <- NewData90$n_clients * NewData90$Cov
    Predict90 <- predict(Fit, NewData90, type = 'response')
    
    Obs <- weighted.mean(PredictObs, w = OldData$DenWgt)
    Prd100 <- weighted.mean(Predict100, w = NewData100$DenWgt)
    Prd90 <- weighted.mean(Predict90, w = NewData90$DenWgt)
    
    RR1 <- (Obs - Prd100) / Prd100
    RR90 <- (Prd90 - Prd100) / Prd100
    
    ObservedPrev <- round(Obs * 100, 2)
    PredictedPrev100 <- round(Prd100 * 100, 2)
    RelativeBiasObserved <- round(RR1 * 100, 2)
    RelativeBias90 <- round(RR90 * 100, 2)
    
    Results<-data.frame(snu1 = snu1, Time = Time, HIV_fulladjust = PredictedPrev100, RR_observed = RelativeBiasObserved, RR_90 = RelativeBias90)
    Results_data <- rbind (Results_data, Results)
  }
  
  Results_all <- NULL
  SNU1.list <- unique(data$snu1)
  for (j in 1:length(SNU1.list)){
  OldData <- NewData100 <- NewData90 <- data[data$snu1 == SNU1.list[j],]
  
  OldData$DenWgt <- OldData$n_clients * OldData$Cov
  PredictObs <- predict(Fit, OldData, type = 'response')
  
  NewData100$Cov <- 1
  NewData100$DenWgt <- NewData100$n_clients * NewData100$Cov
  Predict100 <- predict(Fit, NewData100, type = 'response')
  
  NewData90$Cov <- 0.9
  NewData90$DenWgt <- NewData90$n_clients * NewData90$Cov
  Predict90 <- predict(Fit, NewData90, type = 'response')
  
  Obs <- weighted.mean(PredictObs, w = OldData$DenWgt)
  Prd100 <- weighted.mean(Predict100, w = NewData100$DenWgt)
  Prd90 <- weighted.mean(Predict90, w = NewData90$DenWgt)
  
  RR1 <- (Obs - Prd100) / Prd100
  RR90 <- (Prd90 - Prd100) / Prd100
  
  ObservedPrev <- round(Obs * 100, 2)
  PredictedPrev100 <- round(Prd100 * 100, 2)
  RelativeBiasObserved <- round(RR1 * 100, 2)
  RelativeBias90 <- round(RR90 * 100, 2) 
  
  Results <- data.frame(snu1 = SNU1.list[j],  Time = 9999, HIV_fulladjust = PredictedPrev100, RR_observed = RelativeBiasObserved, RR_90 = RelativeBias90)
  Results_all <- rbind(Results_all, Results)
  }
  
  Results_all2 <- NULL
  Time.list <- (unique(data$Time))
  for (i in 1:length(Time.list)){
    
    OldData <- NewData100 <- NewData90 <- data[data$Time == Time.list[i],]
    
    OldData$DenWgt <- OldData$n_clients * OldData$Cov
    PredictObs <- predict(Fit, OldData, type = 'response')
    
    NewData100$Cov <- 1
    NewData100$DenWgt <- NewData100$n_clients * NewData100$Cov
    Predict100 <- predict(Fit, NewData100, type = 'response')
    
    NewData90$Cov <- 0.9
    NewData90$DenWgt <- NewData90$n_clients * NewData90$Cov
    Predict90 <- predict(Fit, NewData90, type = 'response')
    
    Obs <- weighted.mean(PredictObs, w = OldData$DenWgt)
    Prd100 <- weighted.mean(Predict100, w = NewData100$DenWgt)
    Prd90 <- weighted.mean(Predict90, w = NewData90$DenWgt)
    
    RR1 <- (Obs - Prd100) / Prd100
    RR90 <- (Prd90 - Prd100) / Prd100
    
    ObservedPrev <- round(Obs * 100, 2)
    PredictedPrev100 <- round(Prd100 * 100, 2)
    RelativeBiasObserved <- round(RR1 * 100, 2)
    RelativeBias90 <- round(RR90 * 100, 2)
    
    Results<-data.frame(snu1 = "All", Time = Time.list[i], HIV_fulladjust = PredictedPrev100, RR_observed = RelativeBiasObserved, RR_90 = RelativeBias90)
    Results_all2 <- rbind (Results_all2, Results)
  }

  data$snu1.Year <- paste(data$snu1, data$Year, sep = "-")
  
  Results_snuyear <- NULL
  SNU1Year.list <- (unique(data$snu1.Year))
  for (i in 1:length(SNU1Year.list)){
    
    OldData <- NewData100 <- NewData90 <- data[data$snu1.Year == SNU1Year.list[i],]
    
    Time <- paste(OldData$Year[1], 99, sep = "")
    snu1 <- OldData$snu1[1]
    OldData$DenWgt <- OldData$n_clients * OldData$Cov
    PredictObs <- predict(Fit, OldData, type = 'response')
    
    NewData100$Cov <- 1
    NewData100$DenWgt <- NewData100$n_clients * NewData100$Cov
    Predict100 <- predict(Fit, NewData100, type = 'response')
    
    NewData90$Cov <- 0.9
    NewData90$DenWgt <- NewData90$n_clients * NewData90$Cov
    Predict90 <- predict(Fit, NewData90, type = 'response')
    
    Obs <- weighted.mean(PredictObs, w = OldData$DenWgt)
    Prd100 <- weighted.mean(Predict100, w = NewData100$DenWgt)
    Prd90 <- weighted.mean(Predict90, w = NewData90$DenWgt)
    
    RR1 <- (Obs - Prd100) / Prd100
    RR90 <- (Prd90 - Prd100) / Prd100
    
    ObservedPrev <- round(Obs * 100, 2)
    PredictedPrev100 <- round(Prd100 * 100, 2)
    RelativeBiasObserved <- round(RR1 * 100, 2)
    RelativeBias90 <- round(RR90 * 100, 2)
    
    Results<-data.frame(snu1 = snu1, Time = Time, HIV_fulladjust = PredictedPrev100, RR_observed = RelativeBiasObserved, RR_90 = RelativeBias90)
    Results_snuyear <- rbind (Results_snuyear, Results)
  }
  
  Results_year <- NULL
  Year.list <- unique(data$Year)
  for (j in 1:length(Year.list)){
    OldData <- NewData100 <- NewData90 <- data[data$Year == Year.list[j],]
    
    OldData$DenWgt <- OldData$n_clients * OldData$Cov
    PredictObs <- predict(Fit, OldData, type = 'response')
    
    NewData100$Cov <- 1
    NewData100$DenWgt <- NewData100$n_clients * NewData100$Cov
    Predict100 <- predict(Fit, NewData100, type = 'response')
    
    NewData90$Cov <- 0.9
    NewData90$DenWgt <- NewData90$n_clients * NewData90$Cov
    Predict90 <- predict(Fit, NewData90, type = 'response')
    
    Obs <- weighted.mean(PredictObs, w = OldData$DenWgt)
    Prd100 <- weighted.mean(Predict100, w = NewData100$DenWgt)
    Prd90 <- weighted.mean(Predict90, w = NewData90$DenWgt)
    
    RR1 <- (Obs - Prd100) / Prd100
    RR90 <- (Prd90 - Prd100) / Prd100
    
    ObservedPrev <- round(Obs * 100, 2)
    PredictedPrev100 <- round(Prd100 * 100, 2)
    RelativeBiasObserved <- round(RR1 * 100, 2)
    RelativeBias90 <- round(RR90 * 100, 2) 
    
    Results <- data.frame(snu1 = "All",  Time = paste(Year.list[j], 99, sep = ""), HIV_fulladjust = PredictedPrev100, RR_observed = RelativeBiasObserved, RR_90 = RelativeBias90)
    Results_year <- rbind(Results_year, Results)
  }
  
  OldData <- NewData100 <- NewData90 <- data
  
  OldData$DenWgt <- OldData$n_clients * OldData$Cov
  PredictObs <- predict(Fit, OldData, type = 'response')
  
  NewData100$Cov <- 1
  NewData100$DenWgt <- NewData100$n_clients * NewData100$Cov
  Predict100 <- predict(Fit, NewData100, type = 'response')
  
  NewData90$Cov <- 0.9
  NewData90$DenWgt <- NewData90$n_clients * NewData90$Cov
  Predict90 <- predict(Fit, NewData90, type = 'response')
  
  Obs <- weighted.mean(PredictObs, w = OldData$DenWgt)
  Prd100 <- weighted.mean(Predict100, w = NewData100$DenWgt)
  Prd90 <- weighted.mean(Predict90, w = NewData90$DenWgt)
  
  RR1 <- (Obs - Prd100) / Prd100
  RR90 <- (Prd90 - Prd100) / Prd100
  
  ObservedPrev <- round(Obs * 100, 2)
  PredictedPrev100 <- round(Prd100 * 100, 2)
  RelativeBiasObserved <- round(RR1 * 100, 2)
  RelativeBias90 <- round(RR90 * 100, 2) 
  
  Results.time <- data.frame(snu1 = "All", Time = 9999, HIV_fulladjust = PredictedPrev100, RR_observed = RelativeBiasObserved, RR_90 = RelativeBias90)
  Results_all2 <- rbind(Results_all2, Results.time)
  
  
  Results_data <- rbind(Results_data, Results_all, Results_all2, Results_snuyear, Results_year)
  
  Results_data <- merge(Results_data, prevalences, by = c("snu1", "Time"))
  Results_data$Time <- as.numeric(Results_data$Time)

  if (bysnu1 == "FALSE" & byperiod == "FALSE" & byyear == "FALSE"){
    Results_data2 <- Results_data[Results_data$snu1 == "All" & Results_data$Time == 9999,]
    return(Results_data2)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "TRUE" & byyear == "TRUE"){
    Results_data3 <- Results_data
    return(Results_data3)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "FALSE" & byyear == "FALSE"){
    Results_data4 <- Results_data[Results_data$Time == 9999,]
    return(Results_data4)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "TRUE" & byyear == "FALSE"){
    Results_data5 <- Results_data[Results_data$snu1 == "All" & Results_data$Time < 10000,]
    return(Results_data5)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "FALSE" & byyear == "TRUE"){
    Results_data6 <- Results_data[Results_data$snu1 == "All" & Results_data$Time > 9998,]
    Results_data6$Time <- ifelse(Results_data6$Time > 9999, str_sub(Results_data6$Time, 1, str_length(Results_data6$Time)-2), 
                                Results_data6$Time)
    return(Results_data6)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "TRUE" & byyear == "FALSE"){
    Results_data7 <- Results_data[Results_data$Time < 10000,]
    return(Results_data7)
  }
  
  if (bysnu1 == "TRUE" & byperiod == "FALSE" & byyear == "TRUE"){
    Results_data8 <- Results_data[Results_data$Time > 9998,]
    Results_data8$Time <- ifelse(Results_data8$Time > 9999, str_sub(Results_data8$Time, 1, str_length(Results_data8$Time)-2), 
                                Results_data8$Time)
    return(Results_data8)
  }
  
  if (bysnu1 == "FALSE" & byperiod == "TRUE" & byyear == "TRUE"){
    Results_data9 <- Results_data[Results_data$snu1 == "All",]
    return(Results_data9)
  }
}




