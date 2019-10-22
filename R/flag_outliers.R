#' Flag outlier observations
#'
#' Flags observations that are considered outliers
#'
#' This function has been developed to flag outlier observations for the following variables: \code{n_clients}, \code{n_status_c}, \code{testpos_c}, \code{testneg_c}, \code{knownpos_c}, 
#' \code{totpos_c}, \code{prv} and \code{cov}.  Outliers are defined as 2 standard deviations greater than or less than the mean value.  
#' 
#' @param data The ANC-RT dataset.  The functions \link[ANCRTAdjust]{name_var}, \link[ANCRTAdjust]{data_clean} and \link[ANCRTAdjust]{mt_adjust} should have been run on the data to properly
#' prepare the data for use here.  The dataset must have the following variables:
#'  \itemize{
#'   \item \code{faciluid}: Facility ID.
#'   \item \code{time}: The time period over which the data was collected.
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
#'   \item \code{prv}:  The HIV prevalence from the specified facility at the specified time period (generated using the \link[ANCRTAdjust]{mt_adjust} function).
#'   \item \code{cov}:  The HIV testing coverage from the specified facility at the specified time period (generated using the \link[ANCRTAdjust]{mt_adjust} function).
#'   \item \code{snu1}: The subnational unit 1 (only required if results are to be flagged by snu1).
#'    }
#' @param flag_by Options include:
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
#'    \code{n_clients}, \code{n_status_c}, \code{testpos_c}, \code{testneg_c}, \code{knownpos_c}, \code{totpos_c}, \code{prv} or \code{cov}. The values 
#'    for each of the eight variables are only reported if they are considered an outlier. If they are not considered an outlier, they are reported 
#'    as "NA". For identification purposes \code{faciluid} and \code{time} are also included.
#'    \item "\code{data}" returns the complete dataset (that was originally input into the function) with the following additional variables:
#'      \itemize{
#'         \item \code{flag_n_clients}: A value of 1 indicates that the \code{n_clients} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag_n_status_c}: A value of 1 indicates that the \code{n_status_c} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag_testpos_c}: A value of 1 indicates that the \code{testpos_c} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag_testneg_c}: A value of 1 indicates that the \code{testneg_c} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag_knownpos_c}: A value of 1 indicates that the \code{knownpos_c} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag_totpos_c}: A value of 1 indicates that the \code{totpos_c} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag_prv}: A value of 1 indicates that the \code{prv} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         \item \code{flag_cov}: A value of 1 indicates that the \code{cov} value is considered an outlier and a value of 0 indicates that it is not considered an outlier.
#'         }
#'  }
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A dataset including either the flagged observations only or the full, original dataset with additional variables indicating flagged observations, according to user inputs.
#'
#' @export
#' 

flag_outliers <- function(data, flag_by = "facility", result = "outliers") {
  
  if (flag_by == "facility") {
    data1 <- NULL
    for (i in levels(as.factor(data$faciluid))) {
      temp <- data[data$faciluid == i,]
      temp$flag_n_clients <- ifelse(((temp$n_clients > (mean(temp$n_clients, na.rm = TRUE) + (2 * sd(temp$n_clients, na.rm = TRUE)))) & !is.na(temp$n_clients)) | 
                                    ((temp$n_clients < (mean(temp$n_clients, na.rm = TRUE) - (2 * sd(temp$n_clients, na.rm = TRUE)))) & !is.na(temp$n_clients)), 1, 0)
      temp$flag_n_status_c <- ifelse(((temp$n_status_c > (mean(temp$n_status_c, na.rm = TRUE) + (2 * sd(temp$n_status_c, na.rm = TRUE)))) & !is.na(temp$n_status_c)) | 
                                    ((temp$n_status_c < (mean(temp$n_status_c, na.rm = TRUE) - (2 * sd(temp$n_status_c, na.rm = TRUE)))) & !is.na(temp$n_status_c)), 1, 0)
      temp$flag_testpos_c <- ifelse(((temp$testpos_c > (mean(temp$testpos_c, na.rm = TRUE) + (2 * sd(temp$testpos_c, na.rm = TRUE)))) & !is.na(temp$testpos_c)) | 
                                 ((temp$testpos_c < (mean(temp$testpos_c, na.rm = TRUE) - (2 * sd(temp$testpos_c, na.rm = TRUE)))) & !is.na(temp$testpos_c)), 1, 0)
      temp$flag_testneg_c <- ifelse(((temp$testneg_c > (mean(temp$testneg_c, na.rm = TRUE) + (2 * sd(temp$testneg_c, na.rm = TRUE)))) & !is.na(temp$testneg_c)) | 
                                  ((temp$testneg_c < (mean(temp$testneg_c, na.rm = TRUE) - (2 * sd(temp$testneg_c, na.rm = TRUE)))) & !is.na(temp$testneg_c)), 1, 0)
      temp$flag_knownpos_c <- ifelse(((temp$knownpos_c > (mean(temp$knownpos_c, na.rm = TRUE) + (2 * sd(temp$knownpos_c, na.rm = TRUE)))) & !is.na(temp$knownpos_c)) | 
                                  ((temp$knownpos_c < (mean(temp$knownpos_c, na.rm = TRUE) - (2 * sd(temp$knownpos_c, na.rm = TRUE)))) & !is.na(temp$knownpos_c)), 1, 0)
      temp$flag_totpos_c <- ifelse(((temp$totpos_c > (mean(temp$totpos_c, na.rm = TRUE) + (2 * sd(temp$totpos_c, na.rm = TRUE)))) & !is.na(temp$totpos_c)) | 
                                  ((temp$totpos_c < (mean(temp$totpos_c, na.rm = TRUE) - (2 * sd(temp$totpos_c, na.rm = TRUE)))) & !is.na(temp$totpos_c)), 1, 0)
      temp$flag_prv <- ifelse(((temp$prv > (mean(temp$prv, na.rm = TRUE) + (2 * sd(temp$prv, na.rm = TRUE)))) & !is.na(temp$prv)) | 
                                 ((temp$prv < (mean(temp$prv, na.rm = TRUE) - (2 * sd(temp$prv, na.rm = TRUE)))) & !is.na(temp$prv)), 1, 0)
      temp$flag_cov <- ifelse(((temp$cov > (mean(temp$cov, na.rm = TRUE) + (2 * sd(temp$cov, na.rm = TRUE)))) & !is.na(temp$cov)) | 
                                 ((temp$cov < (mean(temp$cov, na.rm = TRUE) - (2 * sd(temp$cov, na.rm = TRUE)))) & !is.na(temp$cov)), 1, 0)
      data1 <- rbind(data1, temp)
    }
  
    n_clients_outliers <- subset(data1, flag_n_clients == 1, c(faciluid, time, n_clients))
    n_status_c_outliers <- subset(data1, flag_n_status_c == 1, c(faciluid, time, n_status_c))
    testpos_c_outliers <- subset(data1, flag_testpos_c == 1, c(faciluid, time, testpos_c))
    testneg_c_outliers <- subset(data1, flag_testneg_c == 1, c(faciluid, time, testneg_c))
    knownpos_c_outliers <- subset(data1, flag_knownpos_c == 1, c(faciluid, time, knownpos_c))
    totpos_c_outliers <- subset(data1, flag_totpos_c == 1, c(faciluid, time, totpos_c))
    prv_outliers <- subset(data1, flag_prv == 1, c(faciluid, time, prv))
    cov_outliers <- subset(data1, flag_cov == 1, c(faciluid, time, cov))
  
    resultsa <- merge(n_clients_outliers, n_status_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsb <- merge(resultsa, testpos_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsc <- merge(resultsb, testneg_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsd <- merge(resultsc, knownpos_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultse <- merge(resultsd, totpos_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsf <- merge(resultse, prv_outliers, by = c("faciluid", "time"), all = TRUE)
    results <- merge(resultsf, cov_outliers, by = c("faciluid", "time"), all = TRUE)

    if (result == "outliers") {return(results)}
    if (result == "data") {return(data1)}
  }
  
  if (flag_by == "snu1") {
    data1 <- NULL
    for (i in levels(as.factor(data$snu1))) {
      temp <- data[data$snu1 == i,]
      temp$flag_n_clients <- ifelse(((temp$n_clients > (mean(temp$n_clients, na.rm = TRUE) + (2 * sd(temp$n_clients, na.rm = TRUE)))) & !is.na(temp$n_clients)) | 
                                      ((temp$n_clients < (mean(temp$n_clients, na.rm = TRUE) - (2 * sd(temp$n_clients, na.rm = TRUE)))) & !is.na(temp$n_clients)), 1, 0)
      temp$flag_n_status_c <- ifelse(((temp$n_status_c > (mean(temp$n_status_c, na.rm = TRUE) + (2 * sd(temp$n_status_c, na.rm = TRUE)))) & !is.na(temp$n_status_c)) | 
                                   ((temp$n_status_c < (mean(temp$n_status_c, na.rm = TRUE) - (2 * sd(temp$n_status_c, na.rm = TRUE)))) & !is.na(temp$n_status_c)), 1, 0)
      temp$flag_testpos_c <- ifelse(((temp$testpos_c > (mean(temp$testpos_c, na.rm = TRUE) + (2 * sd(temp$testpos_c, na.rm = TRUE)))) & !is.na(temp$testpos_c)) | 
                                    ((temp$testpos_c < (mean(temp$testpos_c, na.rm = TRUE) - (2 * sd(temp$testpos_c, na.rm = TRUE)))) & !is.na(temp$testpos_c)), 1, 0)
      temp$flag_testneg_c <- ifelse(((temp$testneg_c > (mean(temp$testneg_c, na.rm = TRUE) + (2 * sd(temp$testneg_c, na.rm = TRUE)))) & !is.na(temp$testneg_c)) | 
                                    ((temp$testneg_c < (mean(temp$testneg_c, na.rm = TRUE) - (2 * sd(temp$testneg_c, na.rm = TRUE)))) & !is.na(temp$testneg_c)), 1, 0)
      temp$flag_knownpos_c <- ifelse(((temp$knownpos_c > (mean(temp$knownpos_c, na.rm = TRUE) + (2 * sd(temp$knownpos_c, na.rm = TRUE)))) & !is.na(temp$knownpos_c)) | 
                                     ((temp$knownpos_c < (mean(temp$knownpos_c, na.rm = TRUE) - (2 * sd(temp$knownpos_c, na.rm = TRUE)))) & !is.na(temp$knownpos_c)), 1, 0)
      temp$flag_totpos_c <- ifelse(((temp$totpos_c > (mean(temp$totpos_c, na.rm = TRUE) + (2 * sd(temp$totpos_c, na.rm = TRUE)))) & !is.na(temp$totpos_c)) | 
                                   ((temp$totpos_c < (mean(temp$totpos_c, na.rm = TRUE) - (2 * sd(temp$totpos_c, na.rm = TRUE)))) & !is.na(temp$totpos_c)), 1, 0)
      temp$flag_prv <- ifelse(((temp$prv > (mean(temp$prv, na.rm = TRUE) + (2 * sd(temp$prv, na.rm = TRUE)))) & !is.na(temp$prv)) | 
                                ((temp$prv < (mean(temp$prv, na.rm = TRUE) - (2 * sd(temp$prv, na.rm = TRUE)))) & !is.na(temp$prv)), 1, 0)
      temp$flag_cov <- ifelse(((temp$cov > (mean(temp$cov, na.rm = TRUE) + (2 * sd(temp$cov, na.rm = TRUE)))) & !is.na(temp$cov)) | 
                                ((temp$cov < (mean(temp$cov, na.rm = TRUE) - (2 * sd(temp$cov, na.rm = TRUE)))) & !is.na(temp$cov)), 1, 0)
      data1 <- rbind(data1, temp)
    }
    
    n_clients_outliers <- subset(data1, flag_n_clients == 1, c(faciluid, time, n_clients))
    n_status_c_outliers <- subset(data1, flag_n_status_c == 1, c(faciluid, time, n_status_c))
    testpos_c_outliers <- subset(data1, flag_testpos_c == 1, c(faciluid, time, testpos_c))
    testneg_c_outliers <- subset(data1, flag_testneg_c == 1, c(faciluid, time, testneg_c))
    knownpos_c_outliers <- subset(data1, flag_knownpos_c == 1, c(faciluid, time, knownpos_c))
    totpos_c_outliers <- subset(data1, flag_totpos_c == 1, c(faciluid, time, totpos_c))
    prv_outliers <- subset(data1, flag_prv == 1, c(faciluid, time, prv))
    cov_outliers <- subset(data1, flag_cov == 1, c(faciluid, time, cov))
    
    resultsa <- merge(n_clients_outliers, n_status_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsb <- merge(resultsa, testpos_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsc <- merge(resultsb, testneg_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsd <- merge(resultsc, knownpos_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultse <- merge(resultsd, totpos_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsf <- merge(resultse, prv_outliers, by = c("faciluid", "time"), all = TRUE)
    results <- merge(resultsf, cov_outliers, by = c("faciluid", "time"), all = TRUE)
    
    if (result == "outliers") {return(results)}
    if (result == "data") {return(data1)}
  }
  
  if (flag_by == "country") {
    
    data$flag_n_clients <- ifelse(((data$n_clients > (mean(data$n_clients, na.rm = TRUE) + (2 * sd(data$n_clients, na.rm = TRUE)))) & !is.na(data$n_clients)) | 
                                    ((data$n_clients < (mean(data$n_clients, na.rm = TRUE) - (2 * sd(data$n_clients, na.rm = TRUE)))) & !is.na(data$n_clients)), 1, 0)
    data$flag_n_status_c <- ifelse(((data$n_status_c > (mean(data$n_status_c, na.rm = TRUE) + (2 * sd(data$n_status_c, na.rm = TRUE)))) & !is.na(data$n_status_c)) | 
                                 ((data$n_status_c < (mean(data$n_status_c, na.rm = TRUE) - (2 * sd(data$n_status_c, na.rm = TRUE)))) & !is.na(data$n_status_c)), 1, 0)
    data$flag_testpos_c <- ifelse(((data$testpos_c > (mean(data$testpos_c, na.rm = TRUE) + (2 * sd(data$testpos_c, na.rm = TRUE)))) & !is.na(data$testpos_c)) | 
                                  ((data$testpos_c < (mean(data$testpos_c, na.rm = TRUE) - (2 * sd(data$testpos_c, na.rm = TRUE)))) & !is.na(data$testpos_c)), 1, 0)
    data$flag_testneg_c <- ifelse(((data$testneg_c > (mean(data$testneg_c, na.rm = TRUE) + (2 * sd(data$testneg_c, na.rm = TRUE)))) & !is.na(data$testneg_c)) | 
                                  ((data$testneg_c < (mean(data$testneg_c, na.rm = TRUE) - (2 * sd(data$testneg_c, na.rm = TRUE)))) & !is.na(data$testneg_c)), 1, 0)
    data$flag_knownpos_c <- ifelse(((data$knownpos_c > (mean(data$knownpos_c, na.rm = TRUE) + (2 * sd(data$knownpos_c, na.rm = TRUE)))) & !is.na(data$knownpos_c)) | 
                                   ((data$knownpos_c < (mean(data$knownpos_c, na.rm = TRUE) - (2 * sd(data$knownpos_c, na.rm = TRUE)))) & !is.na(data$knownpos_c)), 1, 0)
    data$flag_totpos_c <- ifelse(((data$totpos_c > (mean(data$totpos_c, na.rm = TRUE) + (2 * sd(data$totpos_c, na.rm = TRUE)))) & !is.na(data$totpos_c)) | 
                                 ((data$totpos_c < (mean(data$totpos_c, na.rm = TRUE) - (2 * sd(data$totpos_c, na.rm = TRUE)))) & !is.na(data$totpos_c)), 1, 0)
    data$flag_prv <- ifelse(((data$prv > (mean(data$prv, na.rm = TRUE) + (2 * sd(data$prv, na.rm = TRUE)))) & !is.na(data$prv)) | 
                              ((data$prv < (mean(data$prv, na.rm = TRUE) - (2 * sd(data$prv, na.rm = TRUE)))) & !is.na(data$prv)), 1, 0)
    data$flag_cov <- ifelse(((data$cov > (mean(data$cov, na.rm = TRUE) + (2 * sd(data$cov, na.rm = TRUE)))) & !is.na(data$cov)) | 
                              ((data$cov < (mean(data$cov, na.rm = TRUE) - (2 * sd(data$cov, na.rm = TRUE)))) & !is.na(data$cov)), 1, 0)

    n_clients_outliers <- subset(data, flag_n_clients == 1, c(faciluid, time, n_clients))
    n_status_c_outliers <- subset(data, flag_n_status_c == 1, c(faciluid, time, n_status_c))
    testpos_c_outliers <- subset(data, flag_testpos_c == 1, c(faciluid, time, testpos_c))
    testneg_c_outliers <- subset(data, flag_testneg_c == 1, c(faciluid, time, testneg_c))
    knownpos_c_outliers <- subset(data, flag_knownpos_c == 1, c(faciluid, time, knownpos_c))
    totpos_c_outliers <- subset(data, flag_totpos_c == 1, c(faciluid, time, totpos_c))
    prv_outliers <- subset(data, flag_prv == 1, c(faciluid, time, prv))
    cov_outliers <- subset(data, flag_cov == 1, c(faciluid, time, cov))
    
    resultsa <- merge(n_clients_outliers, n_status_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsb <- merge(resultsa, testpos_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsc <- merge(resultsb, testneg_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsd <- merge(resultsc, knownpos_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultse <- merge(resultsd, totpos_c_outliers, by = c("faciluid", "time"), all = TRUE)
    resultsf <- merge(resultse, prv_outliers, by = c("faciluid", "time"), all = TRUE)
    results <- merge(resultsf, cov_outliers, by = c("faciluid", "time"), all = TRUE)
    
    if (result == "outliers") {return(results)}
    if (result == "data") {return(data)}
  }  
}
  
  