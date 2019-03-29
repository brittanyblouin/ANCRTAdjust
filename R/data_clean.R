meancov_possible <- function(data){
  data$cov_raw <- data$n_stat / data$n_clients
  meancov <- mean(data$cov_raw[which(data$cov_raw<=1 & !is.na(data$cov_raw))])
  return(meancov)
}

#' Clean ANC-RT Data
#'
#' Cleans ANC-RT data to be used with other functions in this package
#'
#' This function has been developed to clean common issues asoociated with ANC-RT data.  The following tasks
#' are accomplished:
#'  \itemize{
#'    \item \code{ID} is created, corresponding to facility ID, to facilitate cluster analyses.
#'    \item \code{Time} and \code{Year} are properly formatted
#'    \item Age disaggregated data is removed
#'    \item \code{TestNeg} is set to \code{n_status} - \code{knownpos} - \code{testpos} if \code{testneg} is missing.  \code{TestNeg} is set to 0 if negative.
#'    \item \code{KnownPos} is set to \code{n_status} - \code{testneg} - \code{testpos} if \code{knownpos} is missing.  \code{KnownPos} is set to 0 if negative.
#'    \item \code{TestPos} is set to \code{n_status} - \code{testneg} - \code{knownpos} if \code{testpos} is missing.  \code{TestPos} is set to 0 if negative.
#'    \item \code{n_stat} is set to \code{TestPos} + \code{TestNeg} + \code{KnownPos} when all of these variables are available.  If either of the three variables is
#'    missing, \code{n_stat} = \code{n_status}
#'    \item \code{TotPos} (total number of HIV positives) is created as \code{KnownPos} + \code{TestPos}. If either \code{TestPos} or \code{KnownPos} are 
#'    missing but \code{n_stat} and \code{TestNeg} are available, \code{TotPos} = \code{n_stat} - \code{TestNeg}. 
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
#'   }
#'   
#'
#' @param Data_ A country-specific ANC-RT database that has been opened using the \code{parse} function.
#' The following variables (with these variable names) must be included in \code{Data_}:
#'  \itemize{
#'   \item \code{quarter}
#'   \item \code{age}: Age category of pregnant women.
#'   \item \code{faciluid}:  The unique facility identifier.
#'   \item \code{period}
#'   \item \code{year}: The year that the data was collected.
#'   \item \code{testneg}: The number of women from the specified facility, during the specified time period, that tested negative for HIV at their first ANC visit.
#'   \item \code{testpos}: The number of women from the specified facility, during the specified time period, that tested positive for HIV at their first ANC visit.
#'   \item \code{knownpos}: The number of women from the specified facility, during the specified time period, that already knew that they were HIV-positive at their first ANC visit.
#'   \item \code{n_status}:  The number of women from the specified facility, during the specified time period, that had their HIV status ascertained at their first ANC visit, either by testing or through previous knowledge.
#'   \item \code{n_clients}: The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'  }
#'
#' @import stats
#'
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A cleaned ANC-RT database with additional variables:
#'  \itemize{
#'   \item \code{ID}: Facility ID
#'   \item \code{Time}: The quarter that the data was collected
#'   \item \code{Year}: The year that the data was collected
#'   \item \code{TestNeg}: Cleaned \code{testneg} (according to description)
#'   \item \code{TestPos}: Cleaned \code{testpos} (according to description)
#'   \item \code{KnownPos}: Cleaned \code{knownpos} (according to description)
#'   \item \code{n_stat}: Cleaned \code{n_status} (according to description)
#'   \item \code{n_stat.impute}:  Adjusted \code{n_stat} using the impute adjustment option
#'   \item \code{n_stat.remove}:  Adjusted \code{n_stat} using the remove adjustment option
#'   \item \code{n_stat.setmax}:  Adjusted \code{n_stat} using the set to maximum adjustment option
#'   \item \code{TotPos}: Total number of positive HIV cases
#'   \item \code{TotPos.impute}: Adjusted \code{TotPos} if the impute adjustment option for multiple testing is used
#'   \item \code{TotPos.remove}: Adjusted \code{TotPos} if the remove adjustment option for multiple testing is used
#'   \item \code{TotPos.setmax}: Adjusted \code{TotPos} if the set to maximum adjustment option for multiple testing is used
#'  }
#'
#' @export

data_clean <- function(Data_){
  Data_$Time <- with(Data_, ifelse(quarter == 1, year - 0.25,
                              ifelse(quarter == 2, year,
                                ifelse(quarter == 3, year + 0.25, year + 0.5))))

  Data_$Year <- with(Data_,ifelse(Time < 2016, 2015,
                             ifelse (Time >= 2016 & Time < 2017, 2016, 2017)))

  Data_ <- Data_[Data_$age == 'all_sum' | Data_$age == 'all', ]
  Data_$tmp <- paste(Data_$faciluid, Data_$period)
  Data_$Ones <- 1
  dupl <- aggregate(Data_$Ones, by = list(Data_$tmp), FUN = sum)
  dupl <- dupl[dupl$x > 1, ]
  Data__tmp1 <- Data_[!(Data_$tmp %in% dupl$Group.1), ]
  Data__tmp2 <- Data_[(Data_$tmp %in% dupl$Group.1) & Data_$age == 'all' & Data_$year < 2017, ]
  Data__tmp3 <- Data_[(Data_$tmp %in% dupl$Group.1) & Data_$year >= 2017 & Data_$age == 'all_sum',  ]
  Data <- rbind(Data__tmp1, Data__tmp2, Data__tmp3)

  Data$TestNeg <- ifelse(!is.na(Data$testneg), Data$testneg,
    Data$n_status - Data$knownpos - Data$testpos)
  Data$TestNeg <- ifelse(Data$TestNeg < 0, 0, Data$TestNeg)

  Data$KnownPos <- ifelse(!is.na(Data$knownpos), Data$knownpos,
    Data$n_status - Data$TestNeg - Data$testpos)
  Data$KnownPos <- ifelse(Data$KnownPos < 0, 0, Data$KnownPos)

  Data$TestPos <- ifelse(!is.na(Data$testpos), Data$testpos,
    Data$n_status - Data$TestNeg - Data$KnownPos)
  Data$TestPos <- ifelse(Data$TestPos < 0, 0, Data$TestPos)

  Data$n_stat <- ifelse(!is.na(Data$TestPos) & !is.na(Data$TestNeg) & !is.na(Data$KnownPos), 
    Data$TestPos + Data$TestNeg + Data$KnownPos, Data$n_status)
  Data$n_stat <- ifelse((Data$n_stat < 0 & !is.na(Data$n_stat)), NA, Data$n_stat)
  
  Data$TotPosA <- Data$KnownPos + Data$TestPos
  Data$TotPosB <- ifelse(is.na(Data$TotPos), Data$n_stat - Data$TestNeg, Data$TotPos)
  Data$TotPosC <- ifelse(Data$TotPosB < 0, 0, Data$TotPosB)
  Data$n_stat <- ifelse(Data$TotPosB < 0 & !is.na(Data$TotPosB), Data$TotPos + Data$TestNeg, Data$n_stat)
  
  Data$ID <- recode.cluster(Data$faciluid)
  
  Data$n_stat.setmax <- ifelse((Data$n_stat > Data$n_clients) & !is.na(Data$n_stat) & !is.na(Data$n_clients), Data$n_clients, Data$n_stat)
  Data$n_stat.remove <- ifelse((Data$n_stat > Data$n_clients) & !is.na(Data$n_stat) & !is.na(Data$n_clients), NA, Data$n_stat)
  
  mean_cov <- ddply(Data, "ID", meancov_possible)
  Data <- merge(Data, mean_cov, by = "ID")
  Data$facilmeancov <- Data$V1
  Data$V1 <- NULL
  Data$n_stat.impute <- ifelse(Data$n_stat > Data$n_clients & !is.na(Data$n_stat) & !is.na(Data$n_clients), round((Data$facilmeancov * Data$n_clients),0), Data$n_stat)
  
  Data$TotPos <- ifelse(Data$n_stat < Data$TotPosC & !is.na(Data$n_stat) & !is.na(Data$TotPosC), NA, Data$TotPosC)
  Data$TotPos.setmax <- ifelse(Data$n_stat.setmax < Data$TotPosC & !is.na(Data$n_stat.setmax) & !is.na(Data$TotPosC), NA, Data$TotPosC)
  Data$TotPos.impute <- ifelse(Data$n_stat.impute < Data$TotPosC & !is.na(Data$n_stat.impute) & !is.na(Data$TotPosC), NA, Data$TotPosC)
  Data$TotPos.remove <- ifelse(Data$n_stat.remove < Data$TotPosC & !is.na(Data$n_stat.remove) & !is.na(Data$TotPosC), NA, Data$TotPosC)
  
  Data$TotPosA <- Data$TotPosB <- Data$TotPosC <- Data$facilmeancov <- Data$Ones <- Data$tmp <- NULL
  
  Data.final <- Data
}
