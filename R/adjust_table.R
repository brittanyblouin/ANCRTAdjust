#' Adjustment factor for imperfect HIV testing coverage at ANC.
#'
#' A table that contains the logit-transformed risk difference estimates for different HIV testing coverage (by 1\% point increments). These were estimated using the President Emergency Funds for AIDS Relief in Africa (PEPFAR) Monitoring, Evaluation, and Reporting (MER) database. Binomial logistic regression models with facility-level fixed effects and marginal standardization were used to assess the effect of testing coverage on HIV prevalence (see Maheu-Giroux et al. (2019) for details on the methods, applied to Malawi). The database contains information on more than 37 millions ANC attendees from 19,527 unique facilities from 17 countries in sub-Saharan Africa, totalling 226,541 observations over the 2015-2019 period. 
#'
#'
#' @format A data frame with 101 rows and 2 variables:
#' \describe{
#'   \item{cov}{the HIV testing coverage}
#'   \item{rd_logit}{the risk difference on the logit scale of HIV prevalence estimate at said coverage level, as compared to what would have been observed at perfect HIV testing coverage}
#'        }
#' @references Maheu-Giroux et al. (2019) AIDS (in press)
"adjust_table"