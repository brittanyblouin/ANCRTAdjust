######################
##Instal the package##
######################
devtools::install_github("brittanyblouin/ANCRTAdjust")
library(ANCRTAdjust)

##################
##Load test data##
##################
data("ancrt")

#####################
##name_var function##
#####################
# The following code will ensure that all variables follow the naming conventions.  Specifically, the variable totpos
# is created as the sum of testpos and knownpos.  Note that negative values (impossible) exist for the variable testpos.
ancrt <- name_var(ancrt, faciluid = "faciluid", time = "time", n_clients = 'n_clients', n_status = "n_status", 
                  knownpos = "knownpos", testpos = "testpos", testneg = "testneg", snu1 = "snu1", Year = "Year")
summary(ancrt)

#######################
##data_clean function##
#######################
# The following code will clean the data according to the process described in the help file.  Note that negative values 
# do not exist for the 'cleaned' testpos variable (testpos_c).  Also note generation of n_status_c.suffix and totpos_c.suffix
# variables for the three multiple testing adjustment options (i.e. suffix = setmax, remove and impute).
ancrt_cleaned <- data_clean(ancrt)
summary(ancrt_cleaned)

#############################
##descriptive_plot function##
#############################
# The following code generates a plot with three sections: 1- % of facilities with data quality problems over time; 2- HIV testing coverage
# over time; and, 3- HIV prevalence over time.
descriptive_plot(ancrt_cleaned)

###############################
##quality_indicators function##
###############################
# The following code calculates the data quality indicators.  There is no missing data in both raw and cleaned data.
# In the raw data, there are 'One or more invalid variables' for 886 (4.43%) of observations in the raw data and for 
# 650 (3.25%) observations in the cleaned data.
ancrt_quality <- quality_indicators(ancrt_cleaned, byregion = FALSE, bytime = FALSE)
ancrt_quality

######################
##mt_adjust function##
######################
# The following code adjusts for multiple testing using the "setmax" option.  Note that n_status_c is now equal
# to n_status_c.setmax (from previous step) and totpos_c is equal to totpos_c.setmax (from previous step).  Also
# note the creation of the variables Cov and Prv.
ancrt_cleaned <- mt_adjust(ancrt_cleaned, "setmax")
summary(ancrt_cleaned)

##########################
##flag_outliers function##
##########################
# The following code flags outliers (values that are more than two standard deviations greater than or less than their facility's 
# mean value).  The first observation with outliers is for facility F-1 at time 4.  Outlier values for this observations include: n_clients = 159, 
# n_status_c = 159 and testneg_c = 109. 
outliers <- flag_outliers(ancrt_cleaned, flagby = "facility", result = "outliers")
head(outliers)

#########################
##HIV_prev_cov function##
#########################
# The following code will calculate HIV prevalence and testing coverage stratified according to user inputs.
# Here, results are not stratified.  HIVraw = 17.07%, HIVprev = 17.11% and HIVcov = 95.55%.
data1 <- HIV_prev_cov(ancrt_cleaned, bysnu1 = FALSE, byperiod = FALSE, byyear = FALSE)
data1

# Here, results are stratified by SNU1 and by year (totalling 12 result lines).  For SNU1 2 in year 2018, for example,
# HIVraw = 17.42, HIVprev = 17.46 and HIVcov = 95.56.
data2 <- HIV_prev_cov(ancrt_cleaned, bysnu1 = TRUE, byperiod = FALSE, byyear = TRUE)
data2 

#########################
##HIVprev_ipcw function##
#########################
# The following code will calculate HIV prevalence and testing coverage adjusted for missing reporting periods,  
# stratified accoding to user inputs.  Here, results are not stratified and the adjusted HIV prevalence is 17.11% and the 
# adjusted testing coverage is 95.55%.
HIVprev_ipcw(ancrt_cleaned, bysnu1 = FALSE, byperiod = FALSE, byyear = FALSE)

#################################
##impcov_adjust_simple function##
#################################
# Adjusting an HIV prevalence of 17.11% due to imperfect testing coverage (when coverage is 95.55%), leads to 
# an adjusted HIV prevalence of 17.0163%.
impcov_adjust_simple(0.1711, 0.9555)

##########################
##impcov_adjust function##
##########################
# Adjusting data1 for it's observed imperfect testing coverage, leads to an adjusted HIV prevalence of 17.02%.
results1 <- impcov_adjust(data1)
results1

# Adjusting data2 for it's observed imperfect testing coverage, leads to an adjusted HIV prevalence for SNU2 2 in 2018 of 17.36%.
results2 <- impcov_adjust(data2)
results2

#############################
##plot_rawadjusted function##
#############################
# Generates a plot of HIV prevalence over four years (2015-2018) for SNU1 2.  Adjusted prevalences are in blue and raw prevalences are shown in red.
plot_rawadjusted(results2, snu1 = 2, time.unit = "YEAR", HIVraw = "TRUE", y.lim = 40)
