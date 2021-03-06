% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impcov_adjust.R
\name{impcov_adjust}
\alias{impcov_adjust}
\title{Adjustment for imperfect HIV testing coverage}
\usage{
impcov_adjust(data = NULL, hiv_prv_point = NULL,
  hiv_cov_point = NULL)
}
\arguments{
\item{data}{The dataframe output from either the \link[ANCRTAdjust]{hiv_prv_cov} function (if adjustment for missing reporting periods is not being performed)
or the \link[ANCRTAdjust]{hiv_prv_ipcw} function (if adjustment for missing reporting periods is also being performed).  The required variables include:
\itemize{
  \item \code{hiv_prv}: The HIV prevalence adjusted for all previous adjustments (e.g. data cleaning, adjustment for multiple testing and/or adjustment for missing reporting periods)
  \item \code{hiv_cov}: The HIV testing coverage adjusted for all previous adjustments (e.g. data cleaning, adjustment for multiple testing and/or adjustment for missing reporting periods)
  }}

\item{hiv_prv_point}{Optional argument if \code{data} is not supplied. if The HIV prevalence estimates to be adjusted for. This MUST be input as a percentage, between between >0 and 100.}

\item{hiv_cov_point}{Optional argument if \code{data} is not supplied.The HIV testing coverage. This MUST be input as a proportion, between >0 and 100.}
}
\value{
A dataframe including the original input data with an additional column, \code{Adjusted_prev}, for the HIV prevalence adjusted for imperfect HIV testing coverage.
}
\description{
Adjusts the HIV prevalence for imperfect HIV testing coverage
}
\details{
This function was designed to adjust HIV prevalence when HIV testing coverage is less than 100\%.  A dataframe with one or many HIV prevalence
and HIV testing coverage estimates is input and all prevalence estimates are adjusted for imperfect HIV testing coverage.  This means that if the input data is stratified by 
\code{time}, \code{period}, and/or \code{year}, the adjustment will also be stratified by the same variable(s). Alternatively, one can supply only point estimates of both HIV prevalence and HIV testing coverage and obtain the adjusted prevalence estimate.

The adjustment factor for specific testing coverage can be found in the \link[ANCRTAdjust]{adjust_table} dataset. These were estimated using the President Emergency Funds for AIDS Relief in Africa (PEPFAR) Monitoring, Evaluation, and Reporting (MER) database. Binomial logistic regression models with facility-level fixed effects and marginal standardization were used to assess the effect of testing coverage on HIV prevalence (see Maheu-Giroux et al. (2019) for details on the methods, applied to Malawi). The database contains information on more than 37 millions ANC attendees from 19,527 unique facilities from 17 countries in sub-Saharan Africa, totalling 226,541 observations over the 2015-2019 period.
}
\author{
Mathieu Maheu-Giroux

Brittany Blouin
}
