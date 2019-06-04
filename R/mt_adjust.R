#' Adjustment for multiple testing
#'
#' Adjusts the variables \code{n_status_c} (the number of women with an ascertained HIV status) and \code{totpos_c} (the total number of women with
#' HIV) when multiple testing is suspected based on the presence of HIV testing coverage values greater than 100\%.
#'
#' This function was designed to adjust the variables \code{n_status_c} and \code{totpos_c}, using one of the three adjustment options, when multiple testing is 
#' suspected (i.e. coverage values greater than 100\% are present).  The three options include: 1) impute; 2) remove; and, 3) set to maximum.
#'
#' @param data The ANC-RT dataset. The functions \link[ANCRTAdjust]{name_var} and \link[ANCRTAdjust]{data_clean} should have been run on the data to properly
#' prepare the data for use here.  The data set must have the following variables:
#'  \itemize{
#'   \item \code{n_status_c}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_status_c.impute}: \code{n_status_c} adjusted using the impute adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_status_c.remove}: \code{n_status_c} adjusted using the remove adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_status_c.setmax}: \code{n_status_c} adjusted using the set to maximum adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{totpos_c}: Cleaned \code{totpos} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{totpos_c.impute}: \code{totpos_c} adjusted using the impute adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{totpos_c.remove}: \code{totpos_c} adjusted using the remove adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{totpos_c.setmax}: \code{totpos_c} adjusted using the set to maximum adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_clients}: The number of women who attended the facility during the time period
#'   }
#' @param adjust_option The adjustment option chosen.  Possible options include:
#'  \itemize{
#'   \item \code{impute}: When \code{n_status_c} > \code{n_clients}, replace \code{n_status_c} with its facility's mean coverage 
#'   (only including valid coverage observations to calculate the mean) multiplied by \code{n_clients}. 
#'   \item \code{remove}: When \code{n_status_c} > \code{n_clients}, replace \code{n_status_c} as missing.
#'   \item \code{setmax}: When \code{n_status_c} > \code{n_clients}, replace \code{n_status_c} with \code{n_clients}.
#'   \item \code{none}: When \code{n_status_c} > \code{n_clients}, no adjustment is made. 
#'   }
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A data set with the variables \code{n_status_c} and \code{totpos_c}, adjusted for multiple testing using the selected adjustment option, is returned.  The 
#' adjustment option variables (e.g. \code{n_status_c.impute}, \code{n_status_c.remove}, \code{totpos_c.impute}, etc.) are removed from the dataset.  Coverage and prevalence
#' (for each facility-time period) are calculated using the newly adjusted variables.
#'
#' @export

mt_adjust <- function(data, adjust_option) {
  
  if (adjust_option == "impute") {
    data$n_status_c <- data$n_status_c.impute
    data$totpos_c <- data$totpos_c.impute
    data$n_status_c.impute <- data$n_status_c.remove <- data$n_status_c.setmax <- NULL
    data$totpos_c.impute <- data$totpos_c.remove <- data$totpos_c.setmax <- NULL
    
  }
  
  if (adjust_option == "remove") {
    data$n_status_c <- data$n_status_c.remove
    data$totpos_c <- data$totpos_c.remove
    data$n_status_c.impute <- data$n_status_c.remove <- data$n_status_c.setmax <- NULL
    data$totpos_c.impute <- data$totpos_c.remove <- data$totpos_c.setmax <- NULL
  }
  
  if (adjust_option == "setmax") {
    data$n_status_c <- data$n_status_c.setmax
    data$totpos_c <- data$totpos_c.setmax
    data$n_status_c.impute <- data$n_status_c.remove <- data$n_status_c.setmax <- NULL
    data$totpos_c.impute <- data$totpos_c.remove <- data$totpos_c.setmax <- NULL
  }
  
  if (adjust_option == "none") {
    data$n_status_c.impute <- data$n_status_c.remove <- data$n_status_c.setmax <- NULL
    data$totpos_c.impute <- data$totpos_c.remove <- data$totpos_c.setmax <- NULL
  }
  
  data$Cov <- ifelse(data$n_clients > 0 & !is.na(data$n_clients), data$n_status_c / data$n_clients, NA)
  data$Prv <- ifelse(data$n_status_c > 0 & !is.na(data$n_status_c), data$totpos_c / data$n_status_c, NA)
  
  return(data)
}