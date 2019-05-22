#' Adjustment for imperfect HIV testing coverage 
#'
#' Adjusts the HIV prevalence for imperfect HIV testing coverage
#' 
#' This function was designed to adjust HIV prevalence when HIV testing coverage is less than 100\%.  A dataframe with one or many HIV prevalence
#' and HIV testing coverage estimates is input and all prevalence estimates are adjusted for imperfect HIV testing coverage.  This means that if the input data is stratified by 
#' \code{time}, \code{period}, or \code{Year}, the adjustment will also be stratifed by the same variable(s). 
#' 
#' @param data The dataframe output from either the \link[ANCRTAdjust]{HIV_prev_cov} function (if adjustment for missing reporting periods is not being performed)
#' or the \link[ANCRTAdjust]{HIVprev_ipcw} function (if adjustment for missing reporting periods is also being performed).  The required variables include:
#' \itemize{
#'   \item \code{HIVprev}: The HIV prevalence
#'   \item \code{HIVcov}: The HIV testing coverage
#'   }
#'  
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A dataframe including the original input data with an additional column, \code{Adjusted_prev}, for the HIV prevalence adjusted for imperfect HIV testing coverage.
#' 
#' @export

impcov_adjust <- function(data) {
  inv.logit <- function(x) {
    val <- 1 / (1 + exp(-x))
    return(val)
  }
  
  logit <- function(x){
    val <- log(x / (1 - x))
  }
  
  data("Adjust_table")
  
  data$HIVcov1 <- data$HIVcov / 100
  data$HIVcov_round <- round(data$HIVcov1, 2)
  
  for (i in 1:length(data$HIVcov_round)) {
    data$RD[i] <- Adjust_table$RD_logit[Adjust_table$Cov == paste(data$HIVcov_round[i])]
  }
  
  data$HIVprev1 <- data$HIVprev / 100
  data$Adjusted_prev <- inv.logit(logit(data$HIVprev1) - data$RD)
  data$Adjusted_prev <- round((data$Adjusted_prev * 100), 2)
  data$HIVcov_round <- data$HIVcov1 <- data$HIVprev1 <- data$RD <- NULL
  return(data)
}