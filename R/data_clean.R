meancov_possible <- function(data) {
  data$cov_raw <- data$n_status_c / data$n_clients
  meancov <- mean(data$cov_raw[which(data$cov_raw <= 1 & !is.na(data$cov_raw))])
  return(meancov)
}

#' Clean ANC-RT Data
#'
#' Cleans ANC-RT data so that it can be used with other functions in the \link[ANCRTAdjust] package.
#'
#' This function has been developed to clean common issues asoociated with ANC-RT data.  The following tasks
#' are accomplished:
#'  \itemize{
#'    \item Age disaggregated data is removed (if applicable).
#'    \item \code{testneg_c} is set to \code{n_status} - \code{knownpos} - \code{testpos} or \code{n_status} - \code{totpos} if \code{testneg} is missing.  \code{testneg_c} is set to 0 if negative.
#'    \item \code{knownpos_c} is set to \code{n_status} - \code{testneg_c} - \code{testpos} or \code{totpos} - \code{testpos} if \code{knownpos} is missing.  \code{knownpos_c} is set to 0 if negative.
#'    \item \code{testpos_c} is set to \code{n_status} - \code{testneg_c} - \code{knownpos_c} or \code{totpos} - \code{knownpos} if \code{testpos} is missing.  \code{testpos_c} is set to 0 if negative.
#'    \item \code{n_status_c} is set to \code{testpos_c} + \code{testneg_c} + \code{knownpos_c} when all of these variables are available.  If either of the three variables is
#'    missing, \code{n_status_c} = \code{n_status}.
#'    \item \code{totpos_c} is set to \code{knownpos_c} + \code{testpos_c} if both of these variables are available. If either \code{testpos_c} or \code{knownpos_c} are 
#'    missing but \code{totpos} is available, \code{totpos_c} is set to \code{totpos}.  If \code{totpos_c} is missing but \code{n_status_c} and \code{testneg_c} are available, \code{totpos_c} = \code{n_status_c} - \code{testneg_c}. 
#'    \item The three suggested \code{n_status_c} adjustments are created in the case that \code{n_status_c} > \code{n_clients}:
#'    \itemize{
#'      \item \code{n_status_c.impute} uses the impute adjustment option
#'      \item \code{n_status_c.remove} uses the remove adjustment option
#'      \item \code{n_status_c.setmax} uses the set to maximum adjustment option
#'      }
#'    \item \code{totpos_c} is adjusted if \code{totpos_c} > \code{n_status_c}:
#'      \itemize{
#'        \item \code{totpos_c}: \code{totpos_c} is set to missing when \code{totpos_c} > \code{n_status_c}
#'        \item \code{totpos_c.impute}: \code{totpos_c} is set to missing when \code{totpos_c} > \code{n_status_c.impute}
#'        \item \code{totpos_c.remove}: \code{totpos_c} is set to missing when \code{totpos_c} > \code{n_status_c.remove}
#'        \item \code{totpos_c.setmax}: \code{totpos_c} is set to missing when \code{totpos_c} > \code{n_status_c.setmax}
#'        }
#'    \item The data is checked for duplicates (i.e. more than one observation exists with the same \code{faciluid} and \code{time}).
#'   }
#'   
#' @param data A country-specific ANC-RT database. The function \link[ANCRTAdjust]{name_var} should have been run on the data to properly
#' prepare the data for use here.
#' The following variables (with these variable names) must be included in \code{data}:
#'  \itemize{
#'   \item \code{time}: The time period that the data was collected.
#'   \item \code{faciluid}:  The unique facility identifier.
#'   \item \code{n_clients}: The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'   \item \code{n_status}:  The number of women from the specified facility, during the specified time period, that had their HIV status ascertained at their first ANC visit, either by testing or through previous knowledge.
#'   \item \code{testpos}: The number of women from the specified facility, during the specified time period, that tested positive for HIV at their first ANC visit.
#'   \item \code{knownpos}: The number of women from the specified facility, during the specified time period, that already knew that they were HIV-positive at their first ANC visit.
#'   \item \code{testneg}: The number of women from the specified facility, during the specified time period, that tested negative for HIV at their first ANC visit.
#'   \item \code{totpos}: The number of women from the specific facility, during the specified time period, that were HIV-positive at their first ANC visit. 
#'   \item \code{age}: Age category of pregnant women (optional).
#'  }
#' @param total_age_cat The value of the \code{age} variable indicating all age categories combined (only necessary if age disaggregated data is included).
#'
#' @import stats
#'
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A cleaned ANC-RT database with additional variables:
#'  \itemize{
#'   \item \code{testneg_c}: Cleaned \code{testneg} (according to description)
#'   \item \code{testpos_c}: Cleaned \code{testpos} (according to description)
#'   \item \code{knownpos_c}: Cleaned \code{knownpos} (according to description)
#'   \item \code{n_status_c}: Cleaned \code{n_status} (according to description)
#'   \item \code{n_status_c.impute}:  Adjusted \code{n_status_c} using the impute adjustment option for multiple testing
#'   \item \code{n_status_c.remove}:  Adjusted \code{n_status_c} using the remove adjustment option for multiple testing
#'   \item \code{n_status_c.setmax}:  Adjusted \code{n_status_c} using the set to maximum adjustment option for multiple testing
#'   \item \code{totpos_c}: Cleaned \code{totpos} (according to description)
#'   \item \code{totpos_c.impute}: Adjusted \code{totpos_c} if the impute adjustment option for multiple testing is used
#'   \item \code{totpos_c.remove}: Adjusted \code{totpos_c} if the remove adjustment option for multiple testing is used
#'   \item \code{totpos_c.setmax}: Adjusted \code{totpos_c} if the set to maximum adjustment option for multiple testing is used
#'  }
#'
#' @export

data_clean <- function(data, total_age_cat = NULL){

  data$ID_time <- paste(data$faciluid, data$time, sep = ".")
  data$check <- duplicated(data$ID_time)
  data$check <- ifelse(data$check == "TRUE", 1, 0)
  
  if(sum(data$check > 0) & !is.null(total_age_cat)){
    data <- data[data$age == total_age_cat,]
  }
  
  data$testneg_c <- ifelse(!is.na(data$testneg), data$testneg,
    data$n_status - data$knownpos - data$testpos)
  data$testneg_c <- ifelse(!is.na(data$testneg_c), data$testneg_c,
    data$n_status - data$totpos)
  data$testneg_c <- ifelse(data$testneg_c < 0, 0, data$testneg_c)

  data$knownpos_c <- ifelse(!is.na(data$knownpos), data$knownpos,
    data$n_status - data$testneg_c - data$testpos)
  data$knownpos_c <- ifelse(!is.na(data$knownpos_c), data$knownpos_c,
    data$totpos - data$testpos)
  data$knownpos_c <- ifelse(data$knownpos_c < 0, 0, data$knownpos_c)

  data$testpos_c <- ifelse(!is.na(data$testpos), data$testpos,
    data$n_status - data$testneg_c - data$knownpos_c)
  data$testpos_c <- ifelse(!is.na(data$testpos_c), data$testpos_c,
    data$totpos - data$knownpos_c)
  data$testpos_c <- ifelse(data$testpos_c < 0, 0, data$testpos_c)

  data$n_status_c <- ifelse(!is.na(data$testpos_c) & !is.na(data$testneg_c) & !is.na(data$knownpos_c), 
    data$testpos_c + data$testneg_c + data$knownpos_c, data$n_status)
  data$n_status_c <- ifelse(!is.na(data$n_status_c), data$n_status_c, data$totpos + data$testneg_c)
  data$n_status_c <- ifelse((data$n_status_c < 0 & !is.na(data$n_status_c)), NA, data$n_status_c)
  
  data$TotPosA <- ifelse(!is.na(data$knownpos_c) & !is.na(data$testpos_c), data$knownpos_c + data$testpos_c, 
    data$totpos)
  data$TotPosB <- ifelse(is.na(data$TotPosA), data$n_status_c - data$testneg_c, data$TotPosA)
  data$TotPosC <- ifelse(data$TotPosB < 0 & !is.na(data$TotPosB), 0, data$TotPosB)
  data$n_status_c <- ifelse(data$TotPosB < 0 & !is.na(data$TotPosB), data$TotPosC + data$testneg_c, data$n_status_c)
  
  data$n_status_c.setmax <- ifelse((data$n_status_c > data$n_clients) & !is.na(data$n_status_c) & !is.na(data$n_clients), data$n_clients, data$n_status_c)
  data$n_status_c.remove <- ifelse((data$n_status_c > data$n_clients) & !is.na(data$n_status_c) & !is.na(data$n_clients), NA, data$n_status_c)
  
  mean_cov <- ddply(data, "faciluid", meancov_possible)
  data <- merge(data, mean_cov, by = "faciluid")
  data$facilmeancov <- data$V1
  data$V1 <- NULL
  data$n_status_c.impute <- ifelse(data$n_status_c > data$n_clients & !is.na(data$n_status_c) & !is.na(data$n_clients), round((data$facilmeancov * data$n_clients),0), data$n_status_c)
  
  data$totpos_c <- ifelse(data$n_status_c < data$TotPosC & !is.na(data$n_status_c) & !is.na(data$TotPosC), NA, data$TotPosC)
  data$totpos_c.setmax <- ifelse(data$n_status_c.setmax < data$TotPosC & !is.na(data$n_status_c.setmax) & !is.na(data$TotPosC), NA, data$TotPosC)
  data$totpos_c.impute <- ifelse(data$n_status_c.impute < data$TotPosC & !is.na(data$n_status_c.impute) & !is.na(data$TotPosC), NA, data$TotPosC)
  data$totpos_c.remove <- ifelse(data$n_status_c.remove < data$TotPosC & !is.na(data$n_status_c.remove) & !is.na(data$TotPosC), NA, data$TotPosC)
  
  data$check <- duplicated(data$ID_time)
  data$check <- ifelse(data$check == "TRUE", 1, 0)
  
  if (sum(data$check) > 0) {warning("Duplicate observations exist.  If age-disaggregated data is included, ensure that 'total_age_cat' has been specified.")}
  
  data$ID_time <- data$check <- data$TotPosA <- data$TotPosB <- data$TotPosC <- data$facilmeancov <- NULL
  
  data.final <- data
}
