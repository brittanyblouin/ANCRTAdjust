#' Flag outlier observations
#'
#' Flags observations that are considered outliers
#'
#' This function has been developed to flag outlier observations for the following variables: \code{n_clients}, \code{n_status_c}, \code{testpos_c}, \code{testneg_c}, \code{knownpos_c}, 
#' \code{totpos_c}, \code{Prv} and \code{Cov}.  Outliers are defined as 2 standard deviations greater than or less than the mean value.  
#' 
#' @param data The ANC-RT dataset.  The functions \link[ANCRTAdjust]{name_var}, \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} should have been run on the data to properly
#' prepare the data for use here.  The dataset must have the following variables:
#'  \itemize{
#'   \item \code{faciluid}: Facility ID.
#'   \item \code{time}: The time period that the data was collected.
#'   \item \code{n_clients}: The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'   \item \code{n_status_c}:  The cleaned number of women from the specified facility, during the specified time period, that had their HIV status ascertained at their first ANC visit, either by testing or through previous knowledge
#'   (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{testpos_c}: The cleaned number of women from the specified facility, during the specified time period, that tested positive for HIV at their first ANC visit
#'   (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{knownpos_c}: The cleaned number of women from the specified facility, during the specified time period, that already knew that they were HIV-positive at their first ANC visit
#'   (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{testneg_c}: The cleaned number of women from the specified facility, during the specified time period, that tested negative for HIV at their first ANC visit 
#'   (generated using the \link[ANCRTAdjust]{data_clean} function).
#'   \item \code{totpos_c}: The cleaned total number of positive HIV cases (generated using the \link[ANCRTAdjust]{data_clean} function).
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
#'    \code{n_clients}, \code{n_status_c}, \code{testpos_c}, \code{testneg_c}, \code{knownpos_c}, \code{totpos_c}, \code{Prv} or \code{Cov}. The values 
#'    for each of the eight variables are only reported if they are considered an outlier. If they are not considered an outlier, they are reported 
#'    as "NA". For identification purposes \code{faciluid} and \code{time} are also included.
#'    \item "\code{data}" returns the complete dataset (that was originally input into the function) with the following additional variables:
#'      \itemize{
#'         \item \code{flag.n_clients}: A value of 1 indicates that the \code{n_clients} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.n_status_c}: A value of 1 indicates that the \code{n_status_c} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.testpos_c}: A value of 1 indicates that the \code{testpos_c} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.testneg_c}: A value of 1 indicates that the \code{testneg_c} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.knownpos_c}: A value of 1 indicates that the \code{knownpos_c} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag.totpos_c}: A value of 1 indicates that the \code{totpos_c} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
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

flag_outliers <- function(data, flagby = "facility", result = "outliers") {
  
  if (flagby == "facility") {
    data1 <- NULL
    for (i in levels(as.factor(data$faciluid))) {
      temp <- data[data$faciluid == i,]
      temp$flag.n_clients <- ifelse(((temp$n_clients > (mean(temp$n_clients, na.rm = TRUE) + (2 * sd(temp$n_clients, na.rm = TRUE)))) & !is.na(temp$n_clients)) | 
                                    ((temp$n_clients < (mean(temp$n_clients, na.rm = TRUE) - (2 * sd(temp$n_clients, na.rm = TRUE)))) & !is.na(temp$n_clients)), 1, 0)
      temp$flag.n_status_c <- ifelse(((temp$n_status_c > (mean(temp$n_status_c, na.rm = TRUE) + (2 * sd(temp$n_status_c, na.rm = TRUE)))) & !is.na(temp$n_status_c)) | 
                                    ((temp$n_status_c < (mean(temp$n_status_c, na.rm = TRUE) - (2 * sd(temp$n_status_c, na.rm = TRUE)))) & !is.na(temp$n_status_c)), 1, 0)
      temp$flag.testpos_c <- ifelse(((temp$testpos_c > (mean(temp$testpos_c, na.rm = TRUE) + (2 * sd(temp$testpos_c, na.rm = TRUE)))) & !is.na(temp$testpos_c)) | 
                                 ((temp$testpos_c < (mean(temp$testpos_c, na.rm = TRUE) - (2 * sd(temp$testpos_c, na.rm = TRUE)))) & !is.na(temp$testpos_c)), 1, 0)
      temp$flag.testneg_c <- ifelse(((temp$testneg_c > (mean(temp$testneg_c, na.rm = TRUE) + (2 * sd(temp$testneg_c, na.rm = TRUE)))) & !is.na(temp$testneg_c)) | 
                                  ((temp$testneg_c < (mean(temp$testneg_c, na.rm = TRUE) - (2 * sd(temp$testneg_c, na.rm = TRUE)))) & !is.na(temp$testneg_c)), 1, 0)
      temp$flag.knownpos_c <- ifelse(((temp$knownpos_c > (mean(temp$knownpos_c, na.rm = TRUE) + (2 * sd(temp$knownpos_c, na.rm = TRUE)))) & !is.na(temp$knownpos_c)) | 
                                  ((temp$knownpos_c < (mean(temp$knownpos_c, na.rm = TRUE) - (2 * sd(temp$knownpos_c, na.rm = TRUE)))) & !is.na(temp$knownpos_c)), 1, 0)
      temp$flag.totpos_c <- ifelse(((temp$totpos_c > (mean(temp$totpos_c, na.rm = TRUE) + (2 * sd(temp$totpos_c, na.rm = TRUE)))) & !is.na(temp$totpos_c)) | 
                                  ((temp$totpos_c < (mean(temp$totpos_c, na.rm = TRUE) - (2 * sd(temp$totpos_c, na.rm = TRUE)))) & !is.na(temp$totpos_c)), 1, 0)
      temp$flag.Prv <- ifelse(((temp$Prv > (mean(temp$Prv, na.rm = TRUE) + (2 * sd(temp$Prv, na.rm = TRUE)))) & !is.na(temp$Prv)) | 
                                 ((temp$Prv < (mean(temp$Prv, na.rm = TRUE) - (2 * sd(temp$Prv, na.rm = TRUE)))) & !is.na(temp$Prv)), 1, 0)
      temp$flag.Cov <- ifelse(((temp$Cov > (mean(temp$Cov, na.rm = TRUE) + (2 * sd(temp$Cov, na.rm = TRUE)))) & !is.na(temp$Cov)) | 
                                 ((temp$Cov < (mean(temp$Cov, na.rm = TRUE) - (2 * sd(temp$Cov, na.rm = TRUE)))) & !is.na(temp$Cov)), 1, 0)
      data1 <- rbind(data1, temp)
    }
  
    n_clients.outliers <- subset(data1, flag.n_clients == 1, c(faciluid, time, n_clients))
    n_status_c.outliers <- subset(data1, flag.n_status_c == 1, c(faciluid, time, n_status_c))
    testpos_c.outliers <- subset(data1, flag.testpos_c == 1, c(faciluid, time, testpos_c))
    testneg_c.outliers <- subset(data1, flag.testneg_c == 1, c(faciluid, time, testneg_c))
    knownpos_c.outliers <- subset(data1, flag.knownpos_c == 1, c(faciluid, time, knownpos_c))
    totpos_c.outliers <- subset(data1, flag.totpos_c == 1, c(faciluid, time, totpos_c))
    Prv.outliers <- subset(data1, flag.Prv == 1, c(faciluid, time, Prv))
    Cov.outliers <- subset(data1, flag.Cov == 1, c(faciluid, time, Cov))
  
    resultsa <- merge(n_clients.outliers, n_status_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsb <- merge(resultsa, testpos_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsc <- merge(resultsb, testneg_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsd <- merge(resultsc, knownpos_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultse <- merge(resultsd, totpos_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsf <- merge(resultse, Prv.outliers, by = c("faciluid", "time"), all = TRUE)
    results <- merge(resultsf, Cov.outliers, by = c("faciluid", "time"), all = TRUE)

    if (result == "outliers") {return(results)}
    if (result == "data") {return(data1)}
  }
  
  if (flagby == "snu1") {
    data1 <- NULL
    for (i in levels(as.factor(data$snu1))) {
      temp <- data[data$snu1 == i,]
      temp$flag.n_clients <- ifelse(((temp$n_clients > (mean(temp$n_clients, na.rm = TRUE) + (2 * sd(temp$n_clients, na.rm = TRUE)))) & !is.na(temp$n_clients)) | 
                                      ((temp$n_clients < (mean(temp$n_clients, na.rm = TRUE) - (2 * sd(temp$n_clients, na.rm = TRUE)))) & !is.na(temp$n_clients)), 1, 0)
      temp$flag.n_status_c <- ifelse(((temp$n_status_c > (mean(temp$n_status_c, na.rm = TRUE) + (2 * sd(temp$n_status_c, na.rm = TRUE)))) & !is.na(temp$n_status_c)) | 
                                   ((temp$n_status_c < (mean(temp$n_status_c, na.rm = TRUE) - (2 * sd(temp$n_status_c, na.rm = TRUE)))) & !is.na(temp$n_status_c)), 1, 0)
      temp$flag.testpos_c <- ifelse(((temp$testpos_c > (mean(temp$testpos_c, na.rm = TRUE) + (2 * sd(temp$testpos_c, na.rm = TRUE)))) & !is.na(temp$testpos_c)) | 
                                    ((temp$testpos_c < (mean(temp$testpos_c, na.rm = TRUE) - (2 * sd(temp$testpos_c, na.rm = TRUE)))) & !is.na(temp$testpos_c)), 1, 0)
      temp$flag.testneg_c <- ifelse(((temp$testneg_c > (mean(temp$testneg_c, na.rm = TRUE) + (2 * sd(temp$testneg_c, na.rm = TRUE)))) & !is.na(temp$testneg_c)) | 
                                    ((temp$testneg_c < (mean(temp$testneg_c, na.rm = TRUE) - (2 * sd(temp$testneg_c, na.rm = TRUE)))) & !is.na(temp$testneg_c)), 1, 0)
      temp$flag.knownpos_c <- ifelse(((temp$knownpos_c > (mean(temp$knownpos_c, na.rm = TRUE) + (2 * sd(temp$knownpos_c, na.rm = TRUE)))) & !is.na(temp$knownpos_c)) | 
                                     ((temp$knownpos_c < (mean(temp$knownpos_c, na.rm = TRUE) - (2 * sd(temp$knownpos_c, na.rm = TRUE)))) & !is.na(temp$knownpos_c)), 1, 0)
      temp$flag.totpos_c <- ifelse(((temp$totpos_c > (mean(temp$totpos_c, na.rm = TRUE) + (2 * sd(temp$totpos_c, na.rm = TRUE)))) & !is.na(temp$totpos_c)) | 
                                   ((temp$totpos_c < (mean(temp$totpos_c, na.rm = TRUE) - (2 * sd(temp$totpos_c, na.rm = TRUE)))) & !is.na(temp$totpos_c)), 1, 0)
      temp$flag.Prv <- ifelse(((temp$Prv > (mean(temp$Prv, na.rm = TRUE) + (2 * sd(temp$Prv, na.rm = TRUE)))) & !is.na(temp$Prv)) | 
                                ((temp$Prv < (mean(temp$Prv, na.rm = TRUE) - (2 * sd(temp$Prv, na.rm = TRUE)))) & !is.na(temp$Prv)), 1, 0)
      temp$flag.Cov <- ifelse(((temp$Cov > (mean(temp$Cov, na.rm = TRUE) + (2 * sd(temp$Cov, na.rm = TRUE)))) & !is.na(temp$Cov)) | 
                                ((temp$Cov < (mean(temp$Cov, na.rm = TRUE) - (2 * sd(temp$Cov, na.rm = TRUE)))) & !is.na(temp$Cov)), 1, 0)
      data1 <- rbind(data1, temp)
    }
    
    n_clients.outliers <- subset(data1, flag.n_clients == 1, c(faciluid, time, n_clients))
    n_status_c.outliers <- subset(data1, flag.n_status_c == 1, c(faciluid, time, n_status_c))
    testpos_c.outliers <- subset(data1, flag.testpos_c == 1, c(faciluid, time, testpos_c))
    testneg_c.outliers <- subset(data1, flag.testneg_c == 1, c(faciluid, time, testneg_c))
    knownpos_c.outliers <- subset(data1, flag.knownpos_c == 1, c(faciluid, time, knownpos_c))
    totpos_c.outliers <- subset(data1, flag.totpos_c == 1, c(faciluid, time, totpos_c))
    Prv.outliers <- subset(data1, flag.Prv == 1, c(faciluid, time, Prv))
    Cov.outliers <- subset(data1, flag.Cov == 1, c(faciluid, time, Cov))
    
    resultsa <- merge(n_clients.outliers, n_status_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsb <- merge(resultsa, testpos_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsc <- merge(resultsb, testneg_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsd <- merge(resultsc, knownpos_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultse <- merge(resultsd, totpos_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsf <- merge(resultse, Prv.outliers, by = c("faciluid", "time"), all = TRUE)
    results <- merge(resultsf, Cov.outliers, by = c("faciluid", "time"), all = TRUE)
    
    if (result == "outliers") {return(results)}
    if (result == "data") {return(data1)}
  }
  
  if (flagby == "country") {
    
    data$flag.n_clients <- ifelse(((data$n_clients > (mean(data$n_clients, na.rm = TRUE) + (2 * sd(data$n_clients, na.rm = TRUE)))) & !is.na(data$n_clients)) | 
                                    ((data$n_clients < (mean(data$n_clients, na.rm = TRUE) - (2 * sd(data$n_clients, na.rm = TRUE)))) & !is.na(data$n_clients)), 1, 0)
    data$flag.n_status_c <- ifelse(((data$n_status_c > (mean(data$n_status_c, na.rm = TRUE) + (2 * sd(data$n_status_c, na.rm = TRUE)))) & !is.na(data$n_status_c)) | 
                                 ((data$n_status_c < (mean(data$n_status_c, na.rm = TRUE) - (2 * sd(data$n_status_c, na.rm = TRUE)))) & !is.na(data$n_status_c)), 1, 0)
    data$flag.testpos_c <- ifelse(((data$testpos_c > (mean(data$testpos_c, na.rm = TRUE) + (2 * sd(data$testpos_c, na.rm = TRUE)))) & !is.na(data$testpos_c)) | 
                                  ((data$testpos_c < (mean(data$testpos_c, na.rm = TRUE) - (2 * sd(data$testpos_c, na.rm = TRUE)))) & !is.na(data$testpos_c)), 1, 0)
    data$flag.testneg_c <- ifelse(((data$testneg_c > (mean(data$testneg_c, na.rm = TRUE) + (2 * sd(data$testneg_c, na.rm = TRUE)))) & !is.na(data$testneg_c)) | 
                                  ((data$testneg_c < (mean(data$testneg_c, na.rm = TRUE) - (2 * sd(data$testneg_c, na.rm = TRUE)))) & !is.na(data$testneg_c)), 1, 0)
    data$flag.knownpos_c <- ifelse(((data$knownpos_c > (mean(data$knownpos_c, na.rm = TRUE) + (2 * sd(data$knownpos_c, na.rm = TRUE)))) & !is.na(data$knownpos_c)) | 
                                   ((data$knownpos_c < (mean(data$knownpos_c, na.rm = TRUE) - (2 * sd(data$knownpos_c, na.rm = TRUE)))) & !is.na(data$knownpos_c)), 1, 0)
    data$flag.totpos_c <- ifelse(((data$totpos_c > (mean(data$totpos_c, na.rm = TRUE) + (2 * sd(data$totpos_c, na.rm = TRUE)))) & !is.na(data$totpos_c)) | 
                                 ((data$totpos_c < (mean(data$totpos_c, na.rm = TRUE) - (2 * sd(data$totpos_c, na.rm = TRUE)))) & !is.na(data$totpos_c)), 1, 0)
    data$flag.Prv <- ifelse(((data$Prv > (mean(data$Prv, na.rm = TRUE) + (2 * sd(data$Prv, na.rm = TRUE)))) & !is.na(data$Prv)) | 
                              ((data$Prv < (mean(data$Prv, na.rm = TRUE) - (2 * sd(data$Prv, na.rm = TRUE)))) & !is.na(data$Prv)), 1, 0)
    data$flag.Cov <- ifelse(((data$Cov > (mean(data$Cov, na.rm = TRUE) + (2 * sd(data$Cov, na.rm = TRUE)))) & !is.na(data$Cov)) | 
                              ((data$Cov < (mean(data$Cov, na.rm = TRUE) - (2 * sd(data$Cov, na.rm = TRUE)))) & !is.na(data$Cov)), 1, 0)

    n_clients.outliers <- subset(data, flag.n_clients == 1, c(faciluid, time, n_clients))
    n_status_c.outliers <- subset(data, flag.n_status_c == 1, c(faciluid, time, n_status_c))
    testpos_c.outliers <- subset(data, flag.testpos_c == 1, c(faciluid, time, testpos_c))
    testneg_c.outliers <- subset(data, flag.testneg_c == 1, c(faciluid, time, testneg_c))
    knownpos_c.outliers <- subset(data, flag.knownpos_c == 1, c(faciluid, time, knownpos_c))
    totpos_c.outliers <- subset(data, flag.totpos_c == 1, c(faciluid, time, totpos_c))
    Prv.outliers <- subset(data, flag.Prv == 1, c(faciluid, time, Prv))
    Cov.outliers <- subset(data, flag.Cov == 1, c(faciluid, time, Cov))
    
    resultsa <- merge(n_clients.outliers, n_status_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsb <- merge(resultsa, testpos_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsc <- merge(resultsb, testneg_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsd <- merge(resultsc, knownpos_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultse <- merge(resultsd, totpos_c.outliers, by = c("faciluid", "time"), all = TRUE)
    resultsf <- merge(resultse, Prv.outliers, by = c("faciluid", "time"), all = TRUE)
    results <- merge(resultsf, Cov.outliers, by = c("faciluid", "time"), all = TRUE)
    
    if (result == "outliers") {return(results)}
    if (result == "data") {return(data)}
  }  
}
  
  