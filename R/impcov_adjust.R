#' Adjustment for imperfect HIV testing coverage 
#'
#' Adjusts the HIV prevalence for imperfect HIV testing coverage
#' 
#' This function was designed to adjust HIV prevalence when HIV testing coverage is less than 100\%.  A dataframe with one or many HIV prevalence
#' and HIV testing coverage estimates is input and all prevalence estimates are adjusted for imperfect HIV testing coverage.  This means that if the input data is stratified by 
#' \code{time}, \code{period}, and/or \code{year}, the adjustment will also be stratifed by the same variable(s). Alternatively, one can supply only point estimates of both HIV prevalence and HIV testing coverage and obtain the adjusted prevalence estimate.
#' 
#' The adjustment factor for specific testing coverage can be found in the \link[ANCRTAdjust]{adjust_table} dataset. These were estimated using the President Emergency Funds for AIDS Relief in Africa (PEPFAR) Monitoring, Evaluation, and Reporting (MER) database. Binomial logistic regression models with facility-level fixed effects and marginal standardization were used to assess the effect of testing coverage on HIV prevalence (see Maheu-Giroux et al. (2019) for details on the methods, applied to Malawi). The database contains information on more than 37 millions ANC attendees from 19,527 unique facilities from 17 countries in sub-Saharan Africa, totalling 226,541 observations over the 2015-2019 period. 
#' 
#' @param data The dataframe output from either the \link[ANCRTAdjust]{hiv_prv_cov} function (if adjustment for missing reporting periods is not being performed)
#' or the \link[ANCRTAdjust]{hiv_prv_ipcw} function (if adjustment for missing reporting periods is also being performed).  The required variables include:
#' \itemize{
#'   \item \code{hiv_prv}: The HIV prevalence adjusted for all previous adjustments (e.g. data cleaning, adjustment for multiple testing and/or adjustment for missing reporting periods)
#'   \item \code{hiv_cov}: The HIV testing coverage adjusted for all previous adjustments (e.g. data cleaning, adjustment for multiple testing and/or adjustment for missing reporting periods)
#'   }
#' @param hiv_prv_point Optional argument if \code{data} is not supplied. if The HIV prevalence estimates to be adjuted for. This MUST be input as a perecentage, between between >0 and 100.
#' @param hiv_cov_point Optional argument if \code{data} is not supplied.The HIV testing coverage. This MUST be input as a proportion, between >0 and 100.
  
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return A dataframe including the original input data with an additional column, \code{Adjusted_prev}, for the HIV prevalence adjusted for imperfect HIV testing coverage.
#' 
#' @export

impcov_adjust <- function(data = NULL, hiv_prv_point = NULL, hiv_cov_point = NULL) {
        if (!is.null(data) & !is.null(hiv_prv_point) & !is.null(hiv_cov_point)) {
          stop("Please supply either the data frame or the point estimates, not both") }  
  
  adjust_table <- ANCRTAdjust::adjust_table
  
  inv_logit <- function(x) {
    val <- 1 / (1 + exp(-x))
    return(val) }
  
  logit <- function(x){
    val <- log(x / (1 - x)) 
    return(val) }
   
    if (is.null(data) & !is.null(hiv_prv_point) & !is.null(hiv_cov_point)) {
      if (hiv_prv_point < 0 | hiv_prv_point > 100 | hiv_cov_point <= 0 | hiv_cov_point > 100) {
          stop("Prevalence and coverage needs to be >0% and <=100%") }  
      hiv_cov_point_round <- round(hiv_cov_point / 100, 2)
      rd <- adjust_table$rd_logit[adjust_table$cov == paste(hiv_cov_point_round)]
      val <- inv_logit(logit(hiv_prv_point / 100) - rd) * 100
    }

  if (!is.null(data) & is.null(hiv_prv_point) & is.null(hiv_cov_point))  {
    if (any(data$hiv_prv <= 0 | data$hiv_prv > 100 | data$hiv_cov <= 0 | data$hiv_cov > 100)) {
      stop("Prevalence and coverage need both to be > 0% and <= 100%") }
    data$hiv_cov_round <- round(data$hiv_cov / 100, 2)
    for (i in 1:nrow(data)) {
      data$rd[i] <- adjust_table$rd_logit[adjust_table$cov == paste(data$hiv_cov_round[i])]
    }
  data$adjusted_prv <- inv_logit(logit((data$hiv_prv / 100) - data$rd)) * 100
  data$hiv_cov_round <- data$hiv_cov1 <- data$hiv_prv1 <- data$RD <- NULL
  val <- data
  }
  return(val) 
}