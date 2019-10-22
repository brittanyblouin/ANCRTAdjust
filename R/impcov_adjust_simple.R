#' Adjustment for imperfect HIV testing coverage
#'
#' Adjusts the HIV prevalence for imperfect HIV testing coverage
#' 
#' This function was designed to adjust HIV prevalence when HIV testing coverage is less than 100\%.
#' 
#' @param hiv_prv The HIV prevalence adjusted for all previous adjustments (e.g. data cleaning, adjustment 
#' for multiple testing and/or adjustment for missing reporting periods).  This MUST be input as a proportion, between 0-1.
#' @param hiv_cov The HIV testing coverage adjusted for all previous adjustments (e.g. data cleaning, adjustment 
#' for multiple testing and/or adjustment for missing reporting periods).  This MUST be input as a proportion, between 0-1.
#' 
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return The HIV prevalence adjusted for imperfect HIV testing coverage.
#' 
#' @export

impcov_adjust_simple <- function(hiv_prv, hiv_cov) {

  
  inv_logit <- function(x) {
    val <- 1 / (1 + exp(-x))
    return(val) }
  
  logit <- function(x){
    val <- log(x / (1 - x)) 
    return(val) }
  
  data("adjust_table")
  hiv_cov <- round(hiv_cov, 2)
  rd <- adjust_table$rd_logit[adjust_table$cov == paste(hiv_cov)]
  adjusted_prv <- inv_logit(logit(hiv_prv) - rd)
  return(adjusted_prv)
}