#' Adjustment for imperfect HIV testing coverage
#'
#' Adjusts the HIV prevalence for imperfect HIV testing coverage
#' 
#' This function was designed to adjust HIV prevalence when HIV testing coverage is less than 100\%.
#' 
#' @param HIVprev The HIV prevalence adjusted for all previous adjustments (e.g. data cleaning, adjustment 
#' for multiple testing and/or adjustment for missing reporting periods).  This MUST be input as a proportion, between 0-1.
#' @param HIVcov The HIV testing coverage adjusted for all previous adjustments (e.g. data cleaning, adjustment 
#' for multiple testing and/or adjustment for missing reporting periods).  This MUST be input as a proportion, between 0-1.
#' 
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return The HIV prevalence adjusted for imperfect HIV testing coverage.
#' 
#' @export

impcov_adjust_simple <- function(HIVprev, HIVcov) {
  inv.logit <- function(x) {
    val <- 1 / (1 + exp(-x))
    return(val)
  }
  
  logit <- function(x){
    val <- log(x / (1 - x))
  }
  
  data("Adjust_table")
  HIVcov <- round(HIVcov, 2)
  RD <- Adjust_table$RD_logit[Adjust_table$Cov == paste(HIVcov)]
  Adjusted_prev <- inv.logit(logit(HIVprev) - RD)
  return(Adjusted_prev)
}