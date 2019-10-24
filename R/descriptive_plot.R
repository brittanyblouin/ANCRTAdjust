#' Plot descriptive data over time
#'
#' Creates plots of HIV prevalence, HIV testing coverage and, the primary data quality indicators over time
#'
#' This function has been developed to create three plots:
#'  \itemize{
#'    \item The two primary data quality indicators over time.  The two primary data quality indicators include: 1) The percent of observations with missing data for \code{n_clients}, 
#'    \code{n_status_c} or \code{totpos_c} (in the cleaned data); and, 2) The percent of observations with invalid values for \code{n_clients}, \code{n_status}, 
#'    \code{testpos} or \code{knownpos} (in the raw data).
#'    \item HIV testing coverage over time.  For comparison purposes, HIV testing coverage is calculated using the raw  and cleaned data and also using the three adjustment 
#'    options for multiple testing (1. Using the impute option; 2. Using the remove option; and, 3. Using the set to maximum option).
#'    \item HIV prevalence over time.  For comparison purposes, HIV prevalence is calculated using the raw  and cleaned data and also using the three adjustment 
#'    options for multiple testing (1. Using the impute option; 2. Using the remove option; and, 3. Using the set to maximum option).
#'  }
#'
#' @param data The ANC-RT dataset. The functions \link[ANCRTAdjust]{name_var} and \link[ANCRTAdjust]{data_clean} should have been run on the data to properly
#' prepare the data for use here.  The dataset must include the following variables:
#'  \itemize{
#'   \item \code{n_clients}: The number of women who attended the specific facility during the specific time period for their first ANC visit
#'   \item \code{n_status}: The number of women who attended the specific facility during the specific time period for their first ANC visit and had their HIV status ascertained
#'   \item \code{n_status_c}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_status_c_impute}: \code{n_status_c} adjusted for multiple testing using the impute adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_status_c_remove}: \code{n_status_c} adjusted for multiple testing using the remove adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_status_c_setmax}: \code{n_status_c} adjusted for multiple testing using the set to maximum adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{testpos}: The number of women who tested positive for HIV at their first ANC visit at the specific facility during the specific time period
#'   \item \code{testneg}: The number of women who tested negative for HIV at their first ANC visit at the specific facility during the specific time period
#'   \item \code{knownpos}: The number of women who attended their first ANC visit at the specific facility during the specific time period with previous knowledge of being HIV positive
#'   \item \code{totpos}: Total number of positive HIV cases 
#'   \item \code{totpos_c}: Cleaned \code{totpos} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{totpos_c_impute}: Adjusted \code{totpos_c} if the impute adjustment option for multiple testing is used (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{totpos_c_remove}: Adjusted \code{totpos_c} if the remove adjustment option for multiple testing is used (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{totpos_c_setmax}: Adjusted \code{totpos_c} if the set to maximum adjustment option for multiple testing is used (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{time}: The time period
#'  }
#' @param ylim_ind The y-axis lower and upper limits of the plot of the primary data quality indicators over time (if left blank, ggplot() defaults are used)
#' @param ylim_cov The y-axis lower and upper limits of the plot of HIV testing coverage over time (if left blank, ggplot() defaults are used)
#' @param ylim_prv The y-axis lower and upper limits of the plot of HIV prevalence over time (if left blank, ggplot() defaults are used)
#' @param plot_type The plots included in the final plot.  Options include:
#'  \itemize{
#'   \item \code{full} All three plots are included (default)
#'   \item \code{prev_cov} Plots of HIV prevalence over time and HIV testing coverage over time are included
#'   \item \code{prev_ind} Plots of HIV prevalence over time and the primary data quality indicators over time are included
#'   \item \code{cov_ind} Plots of HIV testing coverage over time and the primary data quality indicators over time are included
#'   \item \code{prev} The plot of HIV prevalence over time only is included
#'   \item \code{cov} The plot of HIV testing coverage over time only is included
#'   \item \code{ind} The plot of the primary data quality indicators over time only is included
#'   }

#' @import ggplot2 
#' @importFrom plyr ddply
#' @importFrom ggpubr ggarrange
#'
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return Plot(s) showing the desired descriptive data comparing the raw data, the cleaned data and the three different adjustment options for multiple testing.
#'
#' @export

descriptive_plot <- function (data, plot_type = "full") {
  
  if (is.null(data$n_status_c)) {
    stop("Error: please use the data_clean() function prior to producing descriptive plots")
  }

  data$totpos_raw <- ifelse(!is.na(data$knownpos) & !is.na(data$testpos), data$knownpos + data$testpos, 
                            data$totpos)
  
  primary_indicators <- function(data) {
    data$cov_raw <- ifelse(data$n_clients > 0 & !is.na(data$n_clients), (data$n_status / data$n_clients), NA)
    data$impossible_raw <- ifelse(data$cov_raw > 1 & !is.na(data$cov_raw), 1,
                                  ifelse(data$n_status < (data$testneg + data$testpos + data$knownpos) &
                                           !is.na(data$n_status) & !is.na(data$testneg) & !is.na(data$testpos) & !is.na(data$knownpos), 1,
                                         ifelse(data$n_clients < 0 & !is.na(data$n_clients), 1,
                                                ifelse(data$n_status < 0 & !is.na(data$n_status), 1,
                                                       ifelse(data$testpos < 0 & !is.na(data$testpos), 1,
                                                              ifelse(data$knownpos < 0 & !is.na(data$knownpos), 1, 0))))))
    impdata <- round((sum(data$impossible_raw, na.rm = TRUE) / dim(data)[1]) * 100, 2)

    missing <- subset(data, select = c('n_clients', 'n_status', 'totpos_raw'))
    missingdata_raw <- round(((dim(missing)[1] - dim(na.omit(missing))[1]) / dim(missing)[1]) * 100, 2)
    missing2 <- subset(data, select = c('n_clients', 'n_status_c', 'totpos_c'))
    missingdata_cleaned <- round(((dim(missing2)[1] - dim(na.omit(missing2))[1]) / dim(missing2)[1]) * 100, 2)

    primaryindicators <- cbind(impdata, missingdata_raw, missingdata_cleaned)
    return(primaryindicators)
  }
  
  indicators <- plyr::ddply(data, "time", primary_indicators)

  coverages <- function(data) {
    coverage_raw <- (weighted.mean(data$n_status / data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100
    coverage <- (weighted.mean(data$n_status_c / data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100
    coverage_impute <- (weighted.mean(data$n_status_c_impute / data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100
    coverage_remove <- (weighted.mean(data$n_status_c_remove / data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100
    coverage_setmax <- (weighted.mean(data$n_status_c_setmax / data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100
    results <- cbind (coverage_raw, coverage, coverage_impute, coverage_remove, coverage_setmax)
    return(results)
  }
  coverages <- plyr::ddply(data, "time", coverages)
  coverages$dif_raw_clean <- abs(coverages$coverage_raw - coverages$coverage)
  coverages$dif_1.2 <- abs(coverages$coverage - coverages$coverage_impute)
  coverages$dif_1.3 <- abs(coverages$coverage - coverages$coverage_remove)
  coverages$dif_1.4 <- abs(coverages$coverage - coverages$coverage_setmax)
  coverages$dif_2.3 <- abs(coverages$coverage_impute - coverages$coverage_remove)
  coverages$dif_2.4 <- abs(coverages$coverage_impute - coverages$coverage_setmax)
  coverages$dif_3.4 <- abs(coverages$coverage_remove - coverages$coverage_setmax)
  coverages$dif_adj <- pmax(coverages$dif_1.2, coverages$dif_1.3, coverages$dif_1.4, 
                            coverages$dif_2.3, coverages$dif_2.4, coverages$dif_3.4, na.rm = TRUE)
  coverage_raw_clean <- round(mean(coverages$dif_raw_clean, na.rm = TRUE), 3)
  coverage_adjs <- round(mean(coverages$dif_adj, na.rm = TRUE), 3)
  
  hiv_prvs <- function(data) {
    prv_raw <- (weighted.mean((data$totpos_raw) / data$n_status, w = data$n_status, na.rm = TRUE)) * 100
    prv <- (weighted.mean((data$totpos_c) / data$n_status_c, w = data$n_status_c, na.rm = TRUE)) * 100
    prv_impute <- (weighted.mean((data$totpos_c_impute) / data$n_status_c_impute, w = data$n_status_c_impute, na.rm = TRUE)) * 100
    prv_remove <- (weighted.mean((data$totpos_c_remove) / data$n_status_c_remove, w = data$n_status_c_remove, na.rm = TRUE)) * 100
    prv_setmax <- (weighted.mean((data$totpos_c_setmax) / data$n_status_c_setmax, w = data$n_status_c_setmax, na.rm = TRUE)) * 100
    hiv_prvs <- cbind(prv_raw, prv, prv_impute, prv_remove, prv_setmax)
    return(hiv_prvs)
  }
  prevalences <- plyr::ddply(data, "time", hiv_prvs)
  prevalences$dif_raw_clean <- abs(prevalences$prv_raw - prevalences$prv)
  prevalences$dif_1.2 <- abs(prevalences$prv - prevalences$prv_impute)
  prevalences$dif_1.3 <- abs(prevalences$prv - prevalences$prv_remove)
  prevalences$dif_1.4 <- abs(prevalences$prv - prevalences$prv_setmax)
  prevalences$dif_2.3 <- abs(prevalences$prv_impute - prevalences$prv_remove)
  prevalences$dif_2.4 <- abs(prevalences$prv_impute - prevalences$prv_setmax)
  prevalences$dif_3.4 <- abs(prevalences$prv_remove - prevalences$prv_setmax)
  prevalences$dif_adj <- pmax(prevalences$dif_1.2, prevalences$dif_1.3, prevalences$dif_1.4, 
                              prevalences$dif_2.3, prevalences$dif_2.4, prevalences$dif_3.4, na.rm = TRUE)
  prev_raw_clean <- round(mean(prevalences$dif_raw_clean, na.rm = TRUE), 3)
  prev_adjs <- round(mean(prevalences$dif_adj, na.rm = TRUE), 3)
 
  #########
  ##PLOTS##
  #########
  time <- impdata <- missingdata_cleaned <- NULL
  indicator_plot <-
    ggplot(indicators, aes(time)) +
    geom_line(aes(y = impdata, color = "Invalid values in raw data"), size = 1) + 
    geom_point(aes(y = impdata, color = "Invalid values in raw data")) +
    geom_line(aes(y = missingdata_cleaned, color = "Missing data in cleaned data"), size = 1) + 
    geom_point(aes(y = missingdata_cleaned, color = "Missing data in cleaned data")) +
    xlab("") +
    ylab("% facilities with data \n quality problem") +
    theme(axis.title.y = element_text(size = 9)) +
    scale_colour_manual(name = "",
                        values = c("Invalid values in raw data" = "orange", "Missing data in cleaned data" = "lightcoral")) +
    geom_abline(aes(intercept = 10, slope = 0), linetype = "dashed")
  if (exists("ylim_ind")) { 
    indicator_plot <- indicator_plot + ylim(ylim_ind) }
  
  time <- coverage <- coverage_impute <- coverage_setmax <- coverage_remove <- coverage_raw <- NULL
  coverage_plot <-
    ggplot(coverages, aes(time)) +
    geom_line(aes(y = coverage, color = "Cleaned data"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = coverage, color = "Cleaned data"), position = position_jitter(width = 0.02, height = 0)) +
    geom_line(aes(y = coverage_impute, color = "Impute option"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = coverage_impute, color = "Impute option")) +
    geom_line(aes(y = coverage_setmax, color = "Set to maximum option         "), linetype = "dashed", size = 1) + 
    geom_point(aes(y = coverage_setmax, color = "Set to maximum option         ")) +
    geom_line(aes(y = coverage_remove, color = "Remove option"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = coverage_remove, color = "Remove option")) +
    geom_line(aes(y = coverage_raw, color = "Raw data"), size = 1) + 
    geom_point(aes(y = coverage_raw, color = "Raw data")) +
    xlab("") +
    ylab("HIV testing coverage \n (%)") +
    theme(axis.title.y = element_text(size = 9)) +
    scale_colour_manual(name = "",
                      values = c("Raw data" = "red", "Cleaned data" = "darkgoldenrod4",
                                 "Set to maximum option         " = "purple", "Remove option" = "blue", "Impute option" = "forestgreen")) +
    geom_abline(aes(intercept = 100, slope = 0), linetype = "dashed")
    if (exists("ylim_cov")) { 
    coverage_plot <- coverage_plot + ylim(ylim_cov) }
  
  time <- prv <- prv_impute <- prv_setmax <- prv_remove <- prv_raw <- NULL 
  prevalence_plot <-
    ggplot(prevalences, aes(time)) +
    geom_line(aes(y = prv, color = "Cleaned data"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = prv, color = "Cleaned data"), position = position_jitter(width = 0.02, height = 0)) +
    geom_line(aes(y = prv_impute, color = "Impute option"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = prv_impute, color = "Impute option")) +
    geom_line(aes(y = prv_setmax, color = "Set to maximum option         "), linetype = "dashed", size = 1) + 
    geom_point(aes(y = prv_setmax, color = "Set to maximum option         ")) +
    geom_line(aes(y = prv_remove, color = "Remove option"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = prv_remove, color = "Remove option")) +
    geom_line(aes(y = prv_raw, color = "Raw data"), size = 1) + 
    geom_point(aes(y = prv_raw, color = "Raw data")) +
    xlab("Time") +
    ylab("HIV prevalence \n (%)") +
    theme(axis.title.y = element_text(size = 9)) +
    scale_colour_manual(name = "",
                      values = c("Raw data" = "red", "Cleaned data" = "darkgoldenrod4",
                                 "Set to maximum option         " = "purple", "Remove option" = "blue", "Impute option" = "forestgreen"))
    if (exists("ylim_prv")) { 
    prevalence_plot <- prevalence_plot + ylim(ylim_prv) }

   if(plot_type == "full"){
     finalplot1 <- ggpubr::ggarrange(indicator_plot, coverage_plot, prevalence_plot, ncol = 1, nrow = 3, legend = "right")
     return(finalplot1)
   }

   if(plot_type == "prv_cov"){
     finalplot2 <- ggpubr::ggarrange(coverage_plot, prevalence_plot, ncol = 1, nrow = 2, legend = "right")
     return(finalplot2)
   }

   if(plot_type == "prv_ind"){
     finalplot3 <- ggpubr::ggarrange(indicator_plot, prevalence_plot, ncol = 1, nrow = 2, legend = "right")
     return(finalplot3)
   }

   if(plot_type == "cov_ind"){
     finalplot4 <- ggpubr::ggarrange(indicator_plot, coverage_plot, ncol = 1, nrow = 2, legend = "right")
     return(finalplot4)
   }

   if(plot_type == "prv"){
     finalplot5 <- prevalence_plot
     return(finalplot5)
   }

   if(plot_type == "cov"){
     finalplot6 <- coverage_plot
     return(finalplot6)
   }

   if(plot_type == "ind"){
     finalplot7 <- indicator_plot
     return(finalplot7)
   }
}
