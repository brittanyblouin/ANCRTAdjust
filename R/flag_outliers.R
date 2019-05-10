#' Flag outlier observations
#'
#' Flags observations that are considered outliers
#'
#' This function has been developed to flag outlier observations for the following variables: \code{n_clients}, \code{n_stat}, \code{TestPos}, \code{TestNeg}, \code{KnownPos}, 
#' \code{TotPos}, \code{Prv} and \code{Cov}.  Outliers are defined as 2 standard deviations greater than or less than the mean value.  
#' 
#' @param Data The ANC-RT dataset.  The functions \link[ANCRTAdjust]{name_var}, \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} should have been run on the data to properly
#' prepare the data for use here.  The dataset must have the following variables:
#'  \itemize{
#'   \item \code{faciluid}: Facility ID.
#'   \item \code{time}: The time period that the data was collected.
#'   \item \code{n_clients}: The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'   \item \code{n_stat}:  The cleaned number of women from the specified facility, during the specified time period, that had their HIV status ascertained at their first ANC visit, either by testing or through previous knowledge
#'   (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{TestPos}: The cleaned number of women from the specified facility, during the specified time period, that tested positive for HIV at their first ANC visit
#'   (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{KnownPos}: The cleaned number of women from the specified facility, during the specified time period, that already knew that they were HIV-positive at their first ANC visit
#'   (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{TestNeg}: The cleaned number of women from the specified facility, during the specified time period, that tested negative for HIV at their first ANC visit 
#'   (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{TotPos}: The cleaned total number of positive HIV cases (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{Prv}:  The HIV prevalence from the specified facility at the specified time period (generated using the \link[ANCRTAdjust]{mt_adjust} function).
#'   \item \code{Cov}:  The HIV testing coverage from the specified facility at the specified time period (generated using the \link[ANCRTAdjust]{mt_adjust} function).
#'   \item \code{snu1}: The subnational unit 1 (only required if results are to be flagged by snu1).
#'    }
#' @param flagby Options include:
#'  \itemize{
#'    \item "\code{facility}" compares each observation's value to their facility's mean value and flags the observations that 
#'    are greater than or less than 2 standard deviations from the facility mean.
#'    \item "\code{snu1}" compares each observation's value to their sub national unit 1's mean value and flags the observations that 
#'    are greater than or less than 2 standard deviations from the snu1 mean.
#'    \item "\code{country}" compares each observation's value to their country's mean value and flags the observations that 
#'    are greater than or less than 2 standard deviations from the country mean.
#'    }
#' @param result Options include:
#' \itemize{
#'    \item "\code{outliers}" returns a dataset including the observations that are considered to have an outlier value for any of:
#'    \code{n_clients}, \code{n_stat}, \code{TestPos}, \code{TestNeg}, \code{KnownPos}, \code{TotPos}, \code{Prv} or \code{Cov}. The values 
#'    for each of the eight variables are only reported if they are considered an outlier. If they are not considered an outlier, they are reported 
#'    as "NA". For identification purposes \code{faciluid} and \code{time} are also included.
#'    \item "\code{data}" returns the complete dataset (that was originally input into the function) with the following additional variables:
#'      \itemize{
#'         \item \code{flag.n_clients}: A value of 1 indicates that the \code{n_clients} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.n_stat}: A value of 1 indicates that the \code{n_stat} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.TestPos}: A value of 1 indicates that the \code{TestPos} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.TestNeg}: A value of 1 indicates that the \code{TestNeg} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.KnownPos}: A value of 1 indicates that the \code{KnownPos} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.TotPos}: A value of 1 indicates that the \code{TotPos} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.Prv}: A value of 1 indicates that the \code{Prv} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.Cov}: A value of 1 indicates that the \code{Cov} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         }
#'  }
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A dataset including either the flagged observations only or the full, original dataset with additional variables indicating flagged observations, according to user inputs.
#'
#' @export
#' 

flag_outliers <- function(Data, flagby = "facility", result = "outliers") {
  
  if (flagby == "facility") {
    Data1 <- NULL
    for (i in levels(as.factor(Data$faciluid))) {
      temp <- Data[Data$faciluid == i,]
      temp$flag.n_clients <- ifelse(((temp$n_clients > (mean(temp$n_clients) + (2 * sd(temp$n_clients)))) & !is.na(temp$n_clients)) | 
                                    ((temp$n_clients < (mean(temp$n_clients) - (2 * sd(temp$n_clients)))) & !is.na(temp$n_clients)), 1, 0)
      temp$flag.n_stat <- ifelse(((temp$n_stat > (mean(temp$n_stat) + (2 * sd(temp$n_stat)))) & !is.na(temp$n_stat)) | 
                                    ((temp$n_stat < (mean(temp$n_stat) - (2 * sd(temp$n_stat)))) & !is.na(temp$n_stat)), 1, 0)
      temp$flag.TestPos <- ifelse(((temp$TestPos > (mean(temp$TestPos) + (2 * sd(temp$TestPos)))) & !is.na(temp$TestPos)) | 
                                 ((temp$TestPos < (mean(temp$TestPos) - (2 * sd(temp$TestPos)))) & !is.na(temp$TestPos)), 1, 0)
      temp$flag.TestNeg <- ifelse(((temp$TestNeg > (mean(temp$TestNeg) + (2 * sd(temp$TestNeg)))) & !is.na(temp$TestNeg)) | 
                                  ((temp$TestNeg < (mean(temp$TestNeg) - (2 * sd(temp$TestNeg)))) & !is.na(temp$TestNeg)), 1, 0)
      temp$flag.KnownPos <- ifelse(((temp$KnownPos > (mean(temp$KnownPos) + (2 * sd(temp$KnownPos)))) & !is.na(temp$KnownPos)) | 
                                  ((temp$KnownPos < (mean(temp$KnownPos) - (2 * sd(temp$KnownPos)))) & !is.na(temp$KnownPos)), 1, 0)
      temp$flag.TotPos <- ifelse(((temp$TotPos > (mean(temp$TotPos) + (2 * sd(temp$TotPos)))) & !is.na(temp$TotPos)) | 
                                  ((temp$TotPos < (mean(temp$TotPos) - (2 * sd(temp$TotPos)))) & !is.na(temp$TotPos)), 1, 0)
      temp$flag.Prv <- ifelse(((temp$Prv > (mean(temp$Prv) + (2 * sd(temp$Prv)))) & !is.na(temp$Prv)) | 
                                 ((temp$Prv < (mean(temp$Prv) - (2 * sd(temp$Prv)))) & !is.na(temp$Prv)), 1, 0)
      temp$flag.Cov <- ifelse(((temp$Cov > (mean(temp$Cov) + (2 * sd(temp$Cov)))) & !is.na(temp$Cov)) | 
                                 ((temp$Cov < (mean(temp$Cov) - (2 * sd(temp$Cov)))) & !is.na(temp$Cov)), 1, 0)
      Data1 <- rbind(Data1, temp)
    }
  
    n_clients.outliers <- subset(Data1, flag.n_clients == 1, c(faciluid, time, n_clients))
    n_stat.outliers <- subset(Data1, flag.n_stat == 1, c(faciluid, time, n_stat))
    TestPos.outliers <- subset(Data1, flag.TestPos == 1, c(faciluid, time, TestPos))
    TestNeg.outliers <- subset(Data1, flag.TestNeg == 1, c(faciluid, time, TestNeg))
    KnownPos.outliers <- subset(Data1, flag.KnownPos == 1, c(faciluid, time, KnownPos))
    TotPos.outliers <- subset(Data1, flag.TotPos == 1, c(faciluid, time, TotPos))
    Prv.outliers <- subset(Data1, flag.Prv == 1, c(faciluid, time, Prv))
    Cov.outliers <- subset(Data1, flag.Cov == 1, c(faciluid, time, Cov))
  
    resultsa <- merge(n_clients.outliers, n_stat.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsb <- merge(resultsa, TestPos.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsc <- merge(resultsb, TestNeg.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsd <- merge(resultsc, KnownPos.outliers, by = c("faciluid", "time"), all = TRUE)
    resultse <- merge(resultsd, TotPos.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsf <- merge(resultse, Prv.outliers, by = c("faciluid", "time"), all = TRUE)
    results <- merge(resultsf, Cov.outliers, by = c("faciluid", "time"), all = TRUE)

    if (result == "outliers") {return(results)}
    if (result == "data") {return(Data1)}
  }
  
  if (flagby == "snu1") {
    Data1 <- NULL
    for (i in levels(as.factor(Data$snu1))) {
      temp <- Data[Data$snu1 == i,]
      temp$flag.n_clients <- ifelse(((temp$n_clients > (mean(temp$n_clients) + (2 * sd(temp$n_clients)))) & !is.na(temp$n_clients)) | 
                                      ((temp$n_clients < (mean(temp$n_clients) - (2 * sd(temp$n_clients)))) & !is.na(temp$n_clients)), 1, 0)
      temp$flag.n_stat <- ifelse(((temp$n_stat > (mean(temp$n_stat) + (2 * sd(temp$n_stat)))) & !is.na(temp$n_stat)) | 
                                   ((temp$n_stat < (mean(temp$n_stat) - (2 * sd(temp$n_stat)))) & !is.na(temp$n_stat)), 1, 0)
      temp$flag.TestPos <- ifelse(((temp$TestPos > (mean(temp$TestPos) + (2 * sd(temp$TestPos)))) & !is.na(temp$TestPos)) | 
                                    ((temp$TestPos < (mean(temp$TestPos) - (2 * sd(temp$TestPos)))) & !is.na(temp$TestPos)), 1, 0)
      temp$flag.TestNeg <- ifelse(((temp$TestNeg > (mean(temp$TestNeg) + (2 * sd(temp$TestNeg)))) & !is.na(temp$TestNeg)) | 
                                    ((temp$TestNeg < (mean(temp$TestNeg) - (2 * sd(temp$TestNeg)))) & !is.na(temp$TestNeg)), 1, 0)
      temp$flag.KnownPos <- ifelse(((temp$KnownPos > (mean(temp$KnownPos) + (2 * sd(temp$KnownPos)))) & !is.na(temp$KnownPos)) | 
                                     ((temp$KnownPos < (mean(temp$KnownPos) - (2 * sd(temp$KnownPos)))) & !is.na(temp$KnownPos)), 1, 0)
      temp$flag.TotPos <- ifelse(((temp$TotPos > (mean(temp$TotPos) + (2 * sd(temp$TotPos)))) & !is.na(temp$TotPos)) | 
                                   ((temp$TotPos < (mean(temp$TotPos) - (2 * sd(temp$TotPos)))) & !is.na(temp$TotPos)), 1, 0)
      temp$flag.Prv <- ifelse(((temp$Prv > (mean(temp$Prv) + (2 * sd(temp$Prv)))) & !is.na(temp$Prv)) | 
                                ((temp$Prv < (mean(temp$Prv) - (2 * sd(temp$Prv)))) & !is.na(temp$Prv)), 1, 0)
      temp$flag.Cov <- ifelse(((temp$Cov > (mean(temp$Cov) + (2 * sd(temp$Cov)))) & !is.na(temp$Cov)) | 
                                ((temp$Cov < (mean(temp$Cov) - (2 * sd(temp$Cov)))) & !is.na(temp$Cov)), 1, 0)
      Data1 <- rbind(Data1, temp)
    }
    
    n_clients.outliers <- subset(Data1, flag.n_clients == 1, c(faciluid, time, n_clients))
    n_stat.outliers <- subset(Data1, flag.n_stat == 1, c(faciluid, time, n_stat))
    TestPos.outliers <- subset(Data1, flag.TestPos == 1, c(faciluid, time, TestPos))
    TestNeg.outliers <- subset(Data1, flag.TestNeg == 1, c(faciluid, time, TestNeg))
    KnownPos.outliers <- subset(Data1, flag.KnownPos == 1, c(faciluid, time, KnownPos))
    TotPos.outliers <- subset(Data1, flag.TotPos == 1, c(faciluid, time, TotPos))
    Prv.outliers <- subset(Data1, flag.Prv == 1, c(faciluid, time, Prv))
    Cov.outliers <- subset(Data1, flag.Cov == 1, c(faciluid, time, Cov))
    
    resultsa <- merge(n_clients.outliers, n_stat.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsb <- merge(resultsa, TestPos.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsc <- merge(resultsb, TestNeg.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsd <- merge(resultsc, KnownPos.outliers, by = c("faciluid", "time"), all = TRUE)
    resultse <- merge(resultsd, TotPos.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsf <- merge(resultse, Prv.outliers, by = c("faciluid", "time"), all = TRUE)
    results <- merge(resultsf, Cov.outliers, by = c("faciluid", "time"), all = TRUE)
    
    if (result == "outliers") {return(results)}
    if (result == "data") {return(Data1)}
  }
  
  if (flagby == "country") {
    
    Data$flag.n_clients <- ifelse(((Data$n_clients > (mean(Data$n_clients) + (2 * sd(Data$n_clients)))) & !is.na(Data$n_clients)) | 
                                    ((Data$n_clients < (mean(Data$n_clients) - (2 * sd(Data$n_clients)))) & !is.na(Data$n_clients)), 1, 0)
    Data$flag.n_stat <- ifelse(((Data$n_stat > (mean(Data$n_stat) + (2 * sd(Data$n_stat)))) & !is.na(Data$n_stat)) | 
                                 ((Data$n_stat < (mean(Data$n_stat) - (2 * sd(Data$n_stat)))) & !is.na(Data$n_stat)), 1, 0)
    Data$flag.TestPos <- ifelse(((Data$TestPos > (mean(Data$TestPos) + (2 * sd(Data$TestPos)))) & !is.na(Data$TestPos)) | 
                                  ((Data$TestPos < (mean(Data$TestPos) - (2 * sd(Data$TestPos)))) & !is.na(Data$TestPos)), 1, 0)
    Data$flag.TestNeg <- ifelse(((Data$TestNeg > (mean(Data$TestNeg) + (2 * sd(Data$TestNeg)))) & !is.na(Data$TestNeg)) | 
                                  ((Data$TestNeg < (mean(Data$TestNeg) - (2 * sd(Data$TestNeg)))) & !is.na(Data$TestNeg)), 1, 0)
    Data$flag.KnownPos <- ifelse(((Data$KnownPos > (mean(Data$KnownPos) + (2 * sd(Data$KnownPos)))) & !is.na(Data$KnownPos)) | 
                                   ((Data$KnownPos < (mean(Data$KnownPos) - (2 * sd(Data$KnownPos)))) & !is.na(Data$KnownPos)), 1, 0)
    Data$flag.TotPos <- ifelse(((Data$TotPos > (mean(Data$TotPos) + (2 * sd(Data$TotPos)))) & !is.na(Data$TotPos)) | 
                                 ((Data$TotPos < (mean(Data$TotPos) - (2 * sd(Data$TotPos)))) & !is.na(Data$TotPos)), 1, 0)
    Data$flag.Prv <- ifelse(((Data$Prv > (mean(Data$Prv) + (2 * sd(Data$Prv)))) & !is.na(Data$Prv)) | 
                              ((Data$Prv < (mean(Data$Prv) - (2 * sd(Data$Prv)))) & !is.na(Data$Prv)), 1, 0)
    Data$flag.Cov <- ifelse(((Data$Cov > (mean(Data$Cov) + (2 * sd(Data$Cov)))) & !is.na(Data$Cov)) | 
                              ((Data$Cov < (mean(Data$Cov) - (2 * sd(Data$Cov)))) & !is.na(Data$Cov)), 1, 0)

    n_clients.outliers <- subset(Data, flag.n_clients == 1, c(faciluid, time, n_clients))
    n_stat.outliers <- subset(Data, flag.n_stat == 1, c(faciluid, time, n_stat))
    TestPos.outliers <- subset(Data, flag.TestPos == 1, c(faciluid, time, TestPos))
    TestNeg.outliers <- subset(Data, flag.TestNeg == 1, c(faciluid, time, TestNeg))
    KnownPos.outliers <- subset(Data, flag.KnownPos == 1, c(faciluid, time, KnownPos))
    TotPos.outliers <- subset(Data, flag.TotPos == 1, c(faciluid, time, TotPos))
    Prv.outliers <- subset(Data, flag.Prv == 1, c(faciluid, time, Prv))
    Cov.outliers <- subset(Data, flag.Cov == 1, c(faciluid, time, Cov))
    
    resultsa <- merge(n_clients.outliers, n_stat.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsb <- merge(resultsa, TestPos.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsc <- merge(resultsb, TestNeg.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsd <- merge(resultsc, KnownPos.outliers, by = c("faciluid", "time"), all = TRUE)
    resultse <- merge(resultsd, TotPos.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsf <- merge(resultse, Prv.outliers, by = c("faciluid", "time"), all = TRUE)
    results <- merge(resultsf, Cov.outliers, by = c("faciluid", "time"), all = TRUE)
    
    if (result == "outliers") {return(results)}
    if (result == "data") {return(Data)}
  }  
}
  
  