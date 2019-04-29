#' Plot descriptive data over time
#'
#' Creates plots of HIV prevalence, HIV testing coverage and, the primary data quality indicators over time
#'
#' This function has been developed to create three plots:
#'  \itemize{
#'    \item The two primary data quality indicators over time.  The two primary data quality indicators include: 1) The percent of observations with missing data for \code{n_clients}, 
#'    \code{n_stat} or \code{TotPos} (in the cleaned data); and, 2) The percent of observations with invalid values for \code{n_clients}, \code{n_status}, 
#'    \code{testpos} or \code{knownpos} (in the raw data).
#'    \item HIV testing coverage over time.  For comparison purposes, HIV testing coverage is calculated using the raw  and cleaned data and also using the three adjustment 
#'    options for multiple testing (1. Using the impute option; 2. Using the remove option; and, 3. Using the set to maximum option).
#'    \item HIV prevalence over time.  For comparison purposes, HIV prevalence is calculated using the raw  and cleaned data and also using the three adjustment 
#'    options for multiple testing (1. Using the impute option; 2. Using the remove option; and, 3. Using the set to maximum option).
#'  }
#'
#' @param data The ANC-RT dataset. The function \link[ANCRTAdjust]{data_clean} should have been run on the data to properly
#' prepare the data for use here.  The data set must include the following variables:
#'  \itemize{
#'   \item \code{n_clients}: The number of women who attended the specific facility during the specific time period for their first ANC visit
#'   \item \code{n_status}: The number of women who attended the specific facility during the specific time period for their first ANC visit and had their HIV status ascertained
#'   \item \code{n_stat}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_stat.impute}: \code{n_stat} adjusted for multiple testing using the impute adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_stat.remove}: \code{n_stat} adjusted for multiple testing using the remove adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{n_stat.setmax}: \code{n_stat} adjusted for multiple testing using the set to maximum adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{testpos}: The number of women who tested positive for HIV at their first ANC visit at the specific facility during the specific time period
#'   \item \code{testneg}: The number of women who tested negative for HIV at their first ANC visit at the specific facility during the specific time period
#'   \item \code{knownpos}: The number of women who attended their first ANC visit at the specific facility during the specific time period with previous knowledge of being HIV positive
#'   \item \code{TotPos}: Total number of positive HIV cases (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{TotPos.impute}: Adjusted \code{TotPos} if the impute adjustment option for multiple testing is used (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{TotPos.remove}: Adjusted \code{TotPos} if the remove adjustment option for multiple testing is used (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{TotPos.setmax}: Adjusted \code{TotPos} if the set to maximum adjustment option for multiple testing is used (generated using the \link[ANCRTAdjust]{data_clean} function)
#'   \item \code{Time}: The time period (generated using the \link[ANCRTAdjust]{data_clean} function)
#'  }
#' @param ylim.ind_min The y-axis lower limit of the plot of the primary data quality indicators over time (default = 0)
#' @param ylim.ind_max The y-axis upper limit of the plot of the primary data quality indicators over time (default = 100)
#' @param ylim.cov_min The y-axis lower limit of the plot of HIV testing coverage over time (default = 0)
#' @param ylim.cov_max The y-axis upper limit of the plot of HIV testing coverage over time (default = 100)
#' @param ylim.prev_min The y-axis lower limit of the plot of HIV prevalence over time (default = 0)
#' @param ylim.prev_max The y-axis upper limit of the plot of HIV prevalence over time (default = 100)
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
#'
#' @import plyr ggplot2
#'
#' @author Mathieu Maheu-Giroux
#' @author Brittany Blouin
#'
#' @return Plot(s) showing the desired descriptive data comparing the raw data, the cleaned data and the three different adjustment options for multiple testing.
#'
#' @export

descriptive_plot <- function (data, ylim.ind_min = 0, ylim.ind_max = 100, ylim.cov_min = 0, ylim.cov_max = 100, 
                              ylim.prev_min = 0, ylim.prev_max = 100, plot_type = "full"){

  primary_indicators <- function(data) {
    data$Cov_raw <- ifelse(data$n_clients > 0 & !is.na(data$n_clients), (data$n_status / data$n_clients), NA)
    data$impossible_raw <- ifelse(data$Cov_raw > 1 & !is.na(data$Cov_raw), 1,
                                  ifelse(data$n_status < (data$testneg + data$testpos + data$knownpos) &
                                           !is.na(data$n_status) & !is.na(data$testneg) & !is.na(data$testpos) & !is.na(data$knownpos), 1,
                                         ifelse(data$n_clients < 0 & !is.na(data$n_clients), 1,
                                                ifelse(data$n_status < 0 & !is.na(data$n_status), 1,
                                                       ifelse(data$testpos < 0 & !is.na(data$testpos), 1,
                                                              ifelse(data$knownpos < 0 & !is.na(data$knownpos), 1, 0))))))
    impdata <- round((sum(data$impossible_raw, na.rm = TRUE) / dim(data)[1]) * 100, 2)

    missing <- subset(data, select = c('n_clients', 'n_status', 'knownpos', 'testpos'))
    missingdata_raw <- round(((dim(missing)[1] - dim(na.omit(missing))[1]) / dim(missing)[1]) * 100, 2)

    missing2 <- subset(data, select = c('n_clients', 'n_stat', 'TotPos'))
    missingdata_cleaned <- round(((dim(missing2)[1] - dim(na.omit(missing2))[1]) / dim(missing2)[1]) * 100, 2)

    primaryindicators <- cbind(impdata, missingdata_raw, missingdata_cleaned)
    return(primaryindicators)
  }
  indicators <- plyr::ddply(data, "time", primary_indicators)

  coverages <- function(data){
    coverage_raw <- (weighted.mean(data$n_status/data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100
    coverage <- (weighted.mean(data$n_stat/data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100
    coverage.impute <- (weighted.mean(data$n_stat.impute/data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100
    coverage.remove <- (weighted.mean(data$n_stat.remove/data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100
    coverage.setmax <- (weighted.mean(data$n_stat.setmax/data$n_clients, w = data$n_clients, na.rm = TRUE)) * 100
    results <- cbind (coverage_raw, coverage, coverage.impute, coverage.remove, coverage.setmax)
    return(results)
  }
  coverages <- plyr::ddply(data, "time", coverages)
  coverages$dif_raw.clean <- abs(coverages$coverage_raw - coverages$coverage)
  coverages$dif_1.2 <- abs(coverages$coverage - coverages$coverage.impute)
  coverages$dif_1.3 <- abs(coverages$coverage - coverages$coverage.remove)
  coverages$dif_1.4 <- abs(coverages$coverage - coverages$coverage.setmax)
  coverages$dif_2.3 <- abs(coverages$coverage.impute - coverages$coverage.remove)
  coverages$dif_2.4 <- abs(coverages$coverage.impute - coverages$coverage.setmax)
  coverages$dif_3.4 <- abs(coverages$coverage.remove - coverages$coverage.setmax)
  coverages$dif_adj <- pmax(coverages$dif_1.2, coverages$dif_1.3, coverages$dif_1.4, 
                            coverages$dif_2.3, coverages$dif_2.4, coverages$dif_3.4, na.rm = TRUE)
  coverage_raw.clean <- round(mean(coverages$dif_raw.clean, na.rm = TRUE), 3)
  coverage_adjs <- round(mean(coverages$dif_adj, na.rm = TRUE), 3)
  
  HIVprevs <- function(data){
    prev_raw <- (weighted.mean((data$testpos + data$knownpos) / data$n_status, w = data$n_status, na.rm = TRUE)) * 100
    prev <- (weighted.mean((data$TotPos) / data$n_stat, w = data$n_stat, na.rm = TRUE)) * 100
    prev.impute <- (weighted.mean((data$TotPos.impute) / data$n_stat.impute, w = data$n_stat.impute, na.rm = TRUE)) * 100
    prev.remove <- (weighted.mean((data$TotPos.remove) / data$n_stat.remove, w = data$n_stat.remove, na.rm = TRUE)) * 100
    prev.setmax <- (weighted.mean((data$TotPos.setmax) / data$n_stat.setmax, w = data$n_stat.setmax, na.rm = TRUE)) * 100
    HIVprevs <- cbind(prev_raw, prev, prev.impute, prev.remove, prev.setmax)
    return(HIVprevs)
  }
  prevalences <- plyr::ddply(data, "time", HIVprevs)
  prevalences$dif_raw.clean <- abs(prevalences$prev_raw - prevalences$prev)
  prevalences$dif_1.2 <- abs(prevalences$prev - prevalences$prev.impute)
  prevalences$dif_1.3 <- abs(prevalences$prev - prevalences$prev.remove)
  prevalences$dif_1.4 <- abs(prevalences$prev - prevalences$prev.setmax)
  prevalences$dif_2.3 <- abs(prevalences$prev.impute - prevalences$prev.remove)
  prevalences$dif_2.4 <- abs(prevalences$prev.impute - prevalences$prev.setmax)
  prevalences$dif_3.4 <- abs(prevalences$prev.remove - prevalences$prev.setmax)
  prevalences$dif_adj <- pmax(prevalences$dif_1.2, prevalences$dif_1.3, prevalences$dif_1.4, 
                              prevalences$dif_2.3, prevalences$dif_2.4, prevalences$dif_3.4, na.rm = TRUE)
  prev_raw.clean <- round(mean(prevalences$dif_raw.clean, na.rm = TRUE), 3)
  prev_adjs <- round(mean(prevalences$dif_adj, na.rm = TRUE), 3)
 
  #########
  ##PLOTS##
  #########
  indicator_plot <-
    ggplot(indicators, aes(time)) +
    geom_line(aes(y = impdata, color = "Invalid values in raw data"), size = 1) + 
    geom_point(aes(y = impdata, color = "Invalid values in raw data")) +
    geom_line(aes(y = missingdata_cleaned, color = "Missing data in cleaned data"), size = 1) + 
    geom_point(aes(y = missingdata_cleaned, color = "Missing data in cleaned data")) +
    xlab("") +
    ylab("% facilities with data \n quality problem") +
    ylim(c(ylim.ind_min, ylim.ind_max)) +
    theme(axis.title.y = element_text(size = 9)) +
    scale_colour_manual(name = "",
                        values = c("Invalid values in raw data" = "orange", "Missing data in cleaned data" = "lightcoral")) +
    geom_abline(aes(intercept = 10, slope = 0), linetype = "dashed")
  
  
  coverage_plot <-
    ggplot(coverages, aes(time)) +
    geom_line(aes(y = coverage, color = "Cleaned data"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = coverage, color = "Cleaned data"), position=position_jitter(width = 0.02, height = 0)) +
    geom_line(aes(y = coverage.impute, color = "Impute option"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = coverage.impute, color = "Impute option")) +
    geom_line(aes(y = coverage.setmax, color = "Set to maximum option         "), linetype = "dashed", size = 1) + 
    geom_point(aes(y = coverage.setmax, color = "Set to maximum option         ")) +
    geom_line(aes(y = coverage.remove, color = "Remove option"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = coverage.remove, color = "Remove option")) +
    geom_line(aes(y = coverage_raw, color = "Raw data"), size = 1) + 
    geom_point(aes(y = coverage_raw, color = "Raw data")) +
    ylim(c(ylim.cov_min, ylim.cov_max)) +
    xlab("") +
    ylab("HIV testing coverage \n (%)") +
    theme(axis.title.y = element_text(size = 9)) +
    scale_colour_manual(name = "",
                      values = c("Raw data" = "red", "Cleaned data" = "darkgoldenrod4",
                                 "Set to maximum option         " = "purple", "Remove option" = "blue", "Impute option" = "forestgreen")) +
    geom_abline(aes(intercept = 100, slope = 0), linetype = "dashed")
    
  
  prevalence_plot <-
    ggplot(prevalences, aes(time)) +
    geom_line(aes(y = prev, color = "Cleaned data"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = prev, color = "Cleaned data"), position=position_jitter(width = 0.02, height = 0)) +
    geom_line(aes(y = prev.impute, color = "Impute option"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = prev.impute, color = "Impute option")) +
    geom_line(aes(y = prev.setmax, color = "Set to maximum option         "), linetype = "dashed", size = 1) + 
    geom_point(aes(y = prev.setmax, color = "Set to maximum option         ")) +
    geom_line(aes(y = prev.remove, color = "Remove option"), linetype = "dashed", size = 1) + 
    geom_point(aes(y = prev.remove, color = "Remove option")) +
    geom_line(aes(y = prev_raw, color = "Raw data"), size = 1) + 
    geom_point(aes(y = prev_raw, color = "Raw data")) +
    xlab("Time") +
    ylab("HIV prevalence \n (%)") +
    ylim(c(ylim.prev_min, ylim.prev_max)) +
    theme(axis.title.y = element_text(size = 9)) +
    scale_colour_manual(name = "",
                      values = c("Raw data" = "red", "Cleaned data" = "darkgoldenrod4",
                                 "Set to maximum option         " = "purple", "Remove option" = "blue", "Impute option" = "forestgreen"))
    

   if(plot_type == "full"){
     finalplot1 <- ggpubr::ggarrange(indicator_plot, coverage_plot, prevalence_plot, ncol = 1, nrow = 3, legend = "right")
     return(finalplot1)
   }

   if(plot_type == "prev_cov"){
     finalplot2 <- ggpubr::ggarrange(coverage_plot, prevalence_plot, ncol = 1, nrow = 2, legend = "right")
     return(finalplot2)
   }

   if(plot_type == "prev_ind"){
     finalplot3 <- ggpubr::ggarrange(indicator_plot, prevalence_plot, ncol = 1, nrow = 2, legend = "right")
     return(finalplot3)
   }

   if(plot_type == "cov_ind"){
     finalplot4 <- ggpubr::ggarrange(indicator_plot, coverage_plot, ncol = 1, nrow = 2, legend = "right")
     return(finalplot4)
   }

   if(plot_type == "prev"){
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
