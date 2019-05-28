#' Name ANC-RT variables
#'
#' Renames the variables in the ANC-RT dataset to ensure that they follow the necessary naming conventions
#'
#' This function has been developed to rename variables in the ANC-RT dataset to ensure that they conform to the 
#' naming conventions necessary for use in this package.
#' 
#' @param data A country-specific ANC-RT database.  The following variables must be included in \code{data}:
#'  \itemize{
#'   \item The time period that the data was collected.
#'   \item The unique facility identifier.
#'   \item The number of women from the specified facility, during the specified time period, that had their HIV status ascertained at their first ANC visit, either by testing or through previous knowledge.
#'   \item The number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#'   }
#'  Optional variables that can be included for re-naming in \code{data} include: 
#'   \itemize{
#'    \item The number of women from the specified facility, during the specified time period, that tested negative for HIV at their first ANC visit.
#'    \item The number of women from the specified facility, during the specified time period, that tested positive for HIV at their first ANC visit.
#'    \item The number of women from the specified facility, during the specified time period, that already knew that they were HIV-positive at their first ANC visit.
#'    \item Age category of pregnant women
#'    \item The number of women from the specified facility, during the specified time period, that were HIV-positive at their first ANC visit.
#'    \item The sub-national unit 1
#'    \item The calendar year from which the data was collected
#'    }
#'    
#' @param faciluid The variable name in \code{data} for the unique facility identifier
#' @param time The variable name in \code{data} for the time period that the data was collected
#' @param n_clients The variable name in \code{data} for the number of women from the specified facility, during the specified time period, that attended their first ANC visit.
#' @param n_status The variable name in \code{data} for the number of women from the specified facility, during the specified time period, that had their HIV status ascertained at their first ANC visit, either by testing or through previous knowledge.
#' @param knownpos The variable name in \code{data} for the number of women from the specified facility, during the specified time period, that already knew that they were HIV-positive at their first ANC visit (if available).
#' If this variable is not specified, it will be created and marked as missing for all observations.
#' @param testpos The variable name in \code{data} for the number of women from the specified facility, during the specified time period, that tested positive for HIV at their first ANC visit (if available).
#' If this variable is not specified, it will be created and marked as missing for all observations.
#' @param testneg The variable name in \code{data} for the number of women from the specified facility, during the specified time period, that tested negative for HIV at their first ANC visit (if available).
#' If this variable is not specified, it will be created and marked as missing for all observations.
#' @param totpos The variable name in \code{data} for the number of women from the specified facility, during the specified time period, that were HIV-positive (if available).  If this variable name is not specified it will 
#' be automatically created by summing \code{testpos} and \code{knownpos}.
#' @param age The variable name in \code{data} for the age category of pregnant women (if available).
#' @param snu1 The variable name in \code{data} for the sub-national unit 1 (if available).
#' @param Year The variable name in \code{data} for the sub-national unit 1 (if available).
#'
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return The ANC-RT dataset with variables re-named, ensuring that subsequent package functions will work.
#'
#' @export

name_var <- function(data = NULL, faciluid = NULL, time = NULL, n_clients = NULL, n_status = NULL, 
                     knownpos = NULL, testpos = NULL, testneg = NULL, totpos = NULL, age = NULL, snu1 = NULL, Year = NULL) {
  
  # verifying inputs
  if (is.null(data)) { stop('Provide input data') }
  if (is.null(faciluid)) { stop('Provide name of the facility ID variable') }
  if (is.null(time)) { stop('Provide name of time variable') }
  if (is.null(n_clients)) { stop('Provide name of the variable for the number of women that attended their first ANC visit') }
  if (is.null(n_status)) { stop('Provide name of the variable for the number of women that had their HIV status ascertained, either by testing or through previous knowledge') }
  if (is.null(knownpos)) { 
    data$knownpos <- NA 
    warning("Because no 'knownpos' variable was provided, this variable is assumed to be missing for all observations and 
             the variable 'knownpos' was created reflecting this assumption.")
    }
  if (is.null(testpos)) { 
    data$testpos <- NA 
    warning("Because no 'testpos' variable was provided, this variable is assumed to be missing for all observations and 
             the variable 'testpos' was created reflecting this assumption.")
    }
  if (is.null(testneg)) {
    data$testneg <- NA 
    warning("Because no 'testneg' variable was provided, this variable is assumed to be missing for all observations and 
             the variable 'testneg' was created reflecting this assumption.")
    
    }
  
  # second verification
  if (length(names(data)[names(data) == faciluid]) == 0) { stop("faciluid's name not recognized in data") }
  if (length(names(data)[names(data) == time]) == 0) { stop("time's name not recognized in data") }
  if (length(names(data)[names(data) == n_clients]) == 0) { stop("n_clients's name not recognized in data") }
  if (length(names(data)[names(data) == n_status]) == 0) { stop("n_status's name not recognized in data") }

  # chaging names
  names(data)[names(data) == faciluid] <- "faciluid"
  names(data)[names(data) == time] <- "time"
  names(data)[names(data) == n_clients] <- "n_clients"
  names(data)[names(data) == n_status] <- "n_status"
  names(data)[names(data) == knownpos] <- "knownpos"
  names(data)[names(data) == testpos] <- "testpos"
  names(data)[names(data) == testneg] <- "testneg"
  names(data)[names(data) == totpos] <- "totpos"
  names(data)[names(data) == age] <- "age"
  names(data)[names(data) == snu1] <- "snu1"
  names(data)[names(data) == Year] <- "Year"
  
  
  if (is.null(totpos)) { data$totpos <- data$testpos + data$knownpos }
  
  return(data)
}
