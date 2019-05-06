#' Adjustment for multiple testing
#'
#' Adjusts the variables \code{n_stat} (the number of women with an ascertained HIV status) and \code{TotPos} (the total number of women with
#' HIV) when multiple testing is suspected based on the presence of HIV testing coverage values greater than 100\%.
#'
#' This function was designed to adjust the variables \code{n_stat} and \code{TotPos}, using one of the three adjustment options, when multiple testing is 
#' suspected (i.e. coverage values greater than 100\% are present).  The three options include: 1) impute; 2) remove; and, 3) set to maximum.
#'
#' @param data The ANC-RT dataset. The function \link[ANCRTAdjust]{data_clean} should have been run on the data to properly
#' prepare the data for use here.  The data set must have the following variables:
#'  \itemize{
#'   \item \code{n_stat.impute}: \code{n_stat} adjusted using the impute adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_stat.remove}: \code{n_stat} adjusted using the remove adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_stat.setmax}: \code{n_stat} adjusted using the set to maximum adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{TotPos.impute}: \code{TotPos} adjusted using the impute adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{TotPos.remove}: \code{TotPos} adjusted using the remove adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{TotPos.setmax}: \code{TotPos} adjusted using the set to maximum adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_clients}: The number of women who attended the facility during the time period
#'   }
#' @param adjust_option The adjustment option chosen.  Possible options include:
#'  \itemize{
#'   \item \code{impute}: When \code{n_stat} > \code{n_clients}, replace \code{n_stat} with its facility's mean coverage 
#'   (only including valid coverage observations to calculate the mean) multiplied by \code{n_clients}. 
#'   \item \code{remove}: When \code{n_stat} > \code{n_clients}, replace \code{n_stat} as missing.
#'   \item \code{setmax}: When \code{n_stat} > \code{n_clients}, replace \code{n_stat} with \code{n_clients}.
#'   \item \code{none}: When \code{n_stat} > \code{n_clients}, no adjustment is made. 
#'   }
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A data set with the variables \code{n_stat} and \code{TotPos}, adjusted for multiple testing using the selected adjustment option, is returned.  The 
#' adjustment option variables (e.g. \code{n_stat.impute}, \code{n_stat.remove}, \code{TotPos.impute}, etc.) are removed from the dataset.  Coverage and prevalence
#' (for each facility-time period) are calculated using the newly adjusted variables.
#'
#' @export

mt_adjust <- function(data, adjust_option) {
  
  if (adjust_option == "impute") {
    data$n_stat <- data$n_stat.impute
    data$TotPos <- data$TotPos.impute
    data$n_stat.impute <- data$n_stat.remove <- data$n_stat.setmax <- NULL
    data$TotPos.impute <- data$TotPos.remove <- data$TotPos.setmax <- NULL
    
  }
  
  if (adjust_option == "remove") {
    data$n_stat <- data$n_stat.remove
    data$TotPos <- data$TotPos.remove
    data$n_stat.impute <- data$n_stat.remove <- data$n_stat.setmax <- NULL
    data$TotPos.impute <- data$TotPos.remove <- data$TotPos.setmax <- NULL
  }
  
  if (adjust_option == "setmax") {
    data$n_stat <- data$n_stat.setmax
    data$TotPos <- data$TotPos.setmax
    data$n_stat.impute <- data$n_stat.remove <- data$n_stat.setmax <- NULL
    data$TotPos.impute <- data$TotPos.remove <- data$TotPos.setmax <- NULL
  }
  
  if (adjust_option == "none") {
    data$n_stat.impute <- data$n_stat.remove <- data$n_stat.setmax <- NULL
    data$TotPos.impute <- data$TotPos.remove <- data$TotPos.setmax <- NULL
  }
  
  data$Cov <- ifelse(data$n_clients > 0 & !is.na(data$n_clients), data$n_stat / data$n_clients, NA)
  data$Prv <- ifelse(data$n_stat > 0 & !is.na(data$n_stat), data$TotPos / data$n_stat, NA)
  
  return(data)
}