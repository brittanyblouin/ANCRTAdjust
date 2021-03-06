% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptive_plot.R
\name{descriptive_plot}
\alias{descriptive_plot}
\title{Plot descriptive data over time}
\usage{
descriptive_plot(data, ylim_ind = NULL, ylim_cov = NULL,
  ylim_prv = NULL, plot_type = "full")
}
\arguments{
\item{data}{The ANC-RT dataset. The functions \link[ANCRTAdjust]{check_data} and \link[ANCRTAdjust]{data_clean} should have been run on the data to properly
prepare the data for use here.  The dataset must include the following variables:
 \itemize{
  \item \code{n_clients}: The number of women who attended the specific facility during the specific time period for their first ANC visit
  \item \code{n_status}: The number of women who attended the specific facility during the specific time period for their first ANC visit and had their HIV status ascertained
  \item \code{n_status_c}: Cleaned \code{n_status} (generated using the \link[ANCRTAdjust]{data_clean} function)
  \item \code{n_status_c_impute}: \code{n_status_c} adjusted for multiple testing using the impute adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
  \item \code{n_status_c_remove}: \code{n_status_c} adjusted for multiple testing using the remove adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
  \item \code{n_status_c_setmax}: \code{n_status_c} adjusted for multiple testing using the set to maximum adjustment option (generated using the \link[ANCRTAdjust]{data_clean} function)
  \item \code{testpos}: The number of women who tested positive for HIV at their first ANC visit at the specific facility during the specific time period
  \item \code{testneg}: The number of women who tested negative for HIV at their first ANC visit at the specific facility during the specific time period
  \item \code{knownpos}: The number of women who attended their first ANC visit at the specific facility during the specific time period with previous knowledge of being HIV positive
  \item \code{totpos}: Total number of positive HIV cases 
  \item \code{totpos_c}: Cleaned \code{totpos} (generated using the \link[ANCRTAdjust]{data_clean} function)
  \item \code{totpos_c_impute}: Adjusted \code{totpos_c} if the impute adjustment option for multiple testing is used (generated using the \link[ANCRTAdjust]{data_clean} function)
  \item \code{totpos_c_remove}: Adjusted \code{totpos_c} if the remove adjustment option for multiple testing is used (generated using the \link[ANCRTAdjust]{data_clean} function)
  \item \code{totpos_c_setmax}: Adjusted \code{totpos_c} if the set to maximum adjustment option for multiple testing is used (generated using the \link[ANCRTAdjust]{data_clean} function)
  \item \code{time}: The time period
 }}

\item{ylim_ind}{The y-axis lower and upper limits of the plot of the primary data quality indicators over time (if left blank, ggplot() defaults are used)}

\item{ylim_cov}{The y-axis lower and upper limits of the plot of HIV testing coverage over time (if left blank, ggplot() defaults are used)}

\item{ylim_prv}{The y-axis lower and upper limits of the plot of HIV prevalence over time (if left blank, ggplot() defaults are used)}

\item{plot_type}{The plots included in the final plot.  Options include:
\itemize{
 \item \code{full} All three plots are included (default)
 \item \code{prev_cov} Plots of HIV prevalence over time and HIV testing coverage over time are included
 \item \code{prev_ind} Plots of HIV prevalence over time and the primary data quality indicators over time are included
 \item \code{cov_ind} Plots of HIV testing coverage over time and the primary data quality indicators over time are included
 \item \code{prev} The plot of HIV prevalence over time only is included
 \item \code{cov} The plot of HIV testing coverage over time only is included
 \item \code{ind} The plot of the primary data quality indicators over time only is included
 }}
}
\value{
Plot(s) showing the desired descriptive data comparing the raw data, the cleaned data and the three different adjustment options for multiple testing.
}
\description{
Creates plots of HIV prevalence, HIV testing coverage and, the primary data quality indicators over time
}
\details{
This function has been developed to create three plots:
 \itemize{
   \item The two primary data quality indicators over time.  The two primary data quality indicators include: 1) The percent of observations with missing data for \code{n_clients}, 
   \code{n_status_c} or \code{totpos_c} (in the cleaned data); and, 2) The percent of observations with invalid values for \code{n_clients}, \code{n_status}, 
   \code{testpos} or \code{knownpos} (in the raw data).
   \item HIV testing coverage over time.  For comparison purposes, HIV testing coverage is calculated using the raw  and cleaned data and also using the three adjustment 
   options for multiple testing (1. Using the impute option; 2. Using the remove option; and, 3. Using the set to maximum option).
   \item HIV prevalence over time.  For comparison purposes, HIV prevalence is calculated using the raw  and cleaned data and also using the three adjustment 
   options for multiple testing (1. Using the impute option; 2. Using the remove option; and, 3. Using the set to maximum option).
 }
}
\author{
Mathieu Maheu-Giroux

Brittany Blouin
}
