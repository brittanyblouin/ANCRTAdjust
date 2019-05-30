meancov_possible <- function(data) {
  data$cov_raw <- data$n_stat / data$n_clients
  meancov <- mean(data$cov_raw[which(data$cov_raw <= 1 & !is.na(data$cov_raw))])
  return(meancov)
}

#' Clean ANC-RT Data
#'
#' Cleans ANC-RT data to be used with other functions in this package
#'
#' This function has been developed to clean common issues asoociated with ANC-RT data.  The following tasks
#' are accomplished:
#'  \itemize{
#'    \item Age disaggregated data is removed (if applicable)
#'    \item \code{TestNeg} is set to \code{n_status} - \code{knownpos} - \code{testpos} or \code{n_status} - \code{totpos} if \code{testneg} is missing.  \code{TestNeg} is set to 0 if negative.
#'    \item \code{KnownPos} is set to \code{n_status} - \code{testneg} - \code{testpos} or \code{totpos} - \code{testpos} if \code{knownpos} is missing.  \code{KnownPos} is set to 0 if negative.
#'    \item \code{TestPos} is set to \code{n_status} - \code{testneg} - \code{knownpos} or \code{totpos} - \code{knownpos} if \code{testpos} is missing.  \code{TestPos} is set to 0 if negative.
#'    \item \code{n_stat} is set to \code{TestPos} + \code{TestNeg} + \code{KnownPos} when all of these variables are available.  If either of the three variables is
#'    missing, \code{n_stat} = \code{n_status}
#'    \item \code{TotPos} (total number of HIV positives) is created as \code{KnownPos} + \code{TestPos}. If either \code{TestPos} or \code{KnownPos} are 
#'    missing but \code{totpos} is available, \code{TotPos} is set to \code{totpos}.  If \code{TotPos} is missing but \code{n_stat} and \code{TestNeg} are available, \code{TotPos} = \code{n_stat} - \code{TestNeg}. 
#'    \item The three suggested \code{n_stat} adjustments are created in the case that \code{n_stat} > \code{n_clients}:
#'    \itemize{
#'      \item \code{n_stat.impute} uses the impute adjustment option
#'      \item \code{n_stat.remove} uses the remove adjustment option
#'      \item \code{n_stat.setmax} uses the set to maximum adjustment option
#'      }
#'    \item \code{TotPos} is adjusted if \code{TotPos} > \code{n_stat}:
#'      \itemize{
#'        \item \code{TotPos.impute}: \code{TotPos} is set to missing when \code{TotPos} > \code{n_stat.impute}
#'        \item \code{TotPos.remove}: \code{TotPos} is set to missing when \code{TotPos} > \code{n_stat.remove}
#'        \item \code{TotPos.setmax}: \code{TotPos} is set to missing when \code{TotPos} > \code{n_stat.setmax}
#'        \item \code{TotPos}: \code{TotPos} is set to missing when \code{TotPos} > \code{n_stat}
#'        }
#'    \item The data is checked for duplicates (i.e. more than one observation exists with the same \code{faciluid} and \code{time})
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
#'   \item \code{TestNeg}: Cleaned \code{testneg} (according to description)
#'   \item \code{TestPos}: Cleaned \code{testpos} (according to description)
#'   \item \code{KnownPos}: Cleaned \code{knownpos} (according to description)
#'   \item \code{n_stat}: Cleaned \code{n_status} (according to description)
#'   \item \code{n_stat.impute}:  Adjusted \code{n_stat} using the impute adjustment option
#'   \item \code{n_stat.remove}:  Adjusted \code{n_stat} using the remove adjustment option
#'   \item \code{n_stat.setmax}:  Adjusted \code{n_stat} using the set to maximum adjustment option
#'   \item \code{TotPos}: Cleaned \code{totpos} (according to description)
#'   \item \code{TotPos.impute}: Adjusted \code{TotPos} if the impute adjustment option for multiple testing is used
#'   \item \code{TotPos.remove}: Adjusted \code{TotPos} if the remove adjustment option for multiple testing is used
#'   \item \code{TotPos.setmax}: Adjusted \code{TotPos} if the set to maximum adjustment option for multiple testing is used
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
  
  data$TestNeg <- ifelse(!is.na(data$testneg), data$testneg,
    data$n_status - data$knownpos - data$testpos)
  data$TestNeg <- ifelse(!is.na(data$TestNeg), data$TestNeg,
    data$n_status - data$totpos)
  data$TestNeg <- ifelse(data$TestNeg < 0, 0, data$TestNeg)

  data$KnownPos <- ifelse(!is.na(data$knownpos), data$knownpos,
    data$n_status - data$TestNeg - data$testpos)
  data$KnownPos <- ifelse(!is.na(data$KnownPos), data$KnownPos,
    data$totpos - data$testpos)
  data$KnownPos <- ifelse(data$KnownPos < 0, 0, data$KnownPos)

  data$TestPos <- ifelse(!is.na(data$testpos), data$testpos,
    data$n_status - data$TestNeg - data$KnownPos)
  data$TestPos <- ifelse(!is.na(data$TestPos), data$TestPos,
    data$totpos - data$KnownPos)
  data$TestPos <- ifelse(data$TestPos < 0, 0, data$TestPos)

  data$n_stat <- ifelse(!is.na(data$TestPos) & !is.na(data$TestNeg) & !is.na(data$KnownPos), 
    data$TestPos + data$TestNeg + data$KnownPos, data$n_status)
  data$n_stat <- ifelse(!is.na(data$n_stat), data$n_stat, data$totpos + data$TestNeg)
  data$n_stat <- ifelse((data$n_stat < 0 & !is.na(data$n_stat)), NA, data$n_stat)
  
  data$TotPosA <- ifelse(!is.na(data$KnownPos) & !is.na(data$TestPos), data$KnownPos + data$TestPos, 
    data$totpos)
  data$TotPosB <- ifelse(is.na(data$TotPosA), data$n_stat - data$TestNeg, data$TotPosA)
  data$TotPosC <- ifelse(data$TotPosB < 0 & !is.na(data$TotPosB), 0, data$TotPosB)
  data$n_stat <- ifelse(data$TotPosB < 0 & !is.na(data$TotPosB), data$TotPosC + data$TestNeg, data$n_stat)
  
  data$n_stat.setmax <- ifelse((data$n_stat > data$n_clients) & !is.na(data$n_stat) & !is.na(data$n_clients), data$n_clients, data$n_stat)
  data$n_stat.remove <- ifelse((data$n_stat > data$n_clients) & !is.na(data$n_stat) & !is.na(data$n_clients), NA, data$n_stat)
  
  mean_cov <- ddply(data, "faciluid", meancov_possible)
  data <- merge(data, mean_cov, by = "faciluid")
  data$facilmeancov <- data$V1
  data$V1 <- NULL
  data$n_stat.impute <- ifelse(data$n_stat > data$n_clients & !is.na(data$n_stat) & !is.na(data$n_clients), round((data$facilmeancov * data$n_clients),0), data$n_stat)
  
  data$TotPos <- ifelse(data$n_stat < data$TotPosC & !is.na(data$n_stat) & !is.na(data$TotPosC), NA, data$TotPosC)
  data$TotPos.setmax <- ifelse(data$n_stat.setmax < data$TotPosC & !is.na(data$n_stat.setmax) & !is.na(data$TotPosC), NA, data$TotPosC)
  data$TotPos.impute <- ifelse(data$n_stat.impute < data$TotPosC & !is.na(data$n_stat.impute) & !is.na(data$TotPosC), NA, data$TotPosC)
  data$TotPos.remove <- ifelse(data$n_stat.remove < data$TotPosC & !is.na(data$n_stat.remove) & !is.na(data$TotPosC), NA, data$TotPosC)
  
  data$check <- duplicated(data$ID_time)
  data$check <- ifelse(data$check == "TRUE", 1, 0)
  
  if (sum(data$check) > 0) {warning("Duplicate observations exist.  If age-disagregated data is included, ensure that 'total_age_cat' has been specified.")}
  
  data$ID_time <- data$check <- data$TotPosA <- data$TotPosB <- data$TotPosC <- data$facilmeancov <- NULL
  
  data.final <- data
}
