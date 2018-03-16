# all codes run in RStudio version 1.0.136 and R version R version 3.3.3
# Older and newer versions of RStudio and R have not been tested
rm(list = ls())
package.list <- list("akima","dataRetrieval","data.table","dplyr","ggmap","ggplot2","gridExtra","rstan", 
                     "Kendall","locfit", "lubridate","maps","ncdf4","readr","reshape2","tidyr","epitools")
source('R/load_packages.R') # clear workspace, clear console, load packages
source("R/GetSeasonDate.R")

# load the new reanalysis
# source('R/load_reanalysis.R') # this can take ~ 10 mins to run -- no need to re-run as the processed data is stored in the "Processed_Data" folder

# load the CPC precipitation data and the GCM precipitaton (historic and future)
# also compute the REP events
# source('R/load_GCM_OBS_PRECIP_REP.R') # this can take ~ 15 mins to run -- no need to re-run as the processed data is stored in the "Processed_Data" folder

# plot the grids of the CPC and GCM
source('R/plot_grids.R') # make Fig 1

# look at the streamflow during REP events
source('R/streamflow_REP_connection.R') # make Fig 2

# make the plots to compare the historic RIPS from the observed record vs. the GCMs
source('R/OBS_GCM_REP_diff.R') # make Fig 3

# plot the composites for the REP events -- should think about the Feb 20th cutoff
source("R/make_REP_composites.R") # make Fig 4

# load the GCM atmospheric data
# source('R/load_GCM.R') # this can take ~ 10 mins to run -- no need to re-run as the processed data is stored in the "Processed_Data" folder

# compare the composites for the REP events for the GCM and the reanalysis
source('R/compare_Z_Q_during_REPs.R') # make Fig 5

# now make the indices and plot the time-series of these indices before and after the events of interest
source("R/make_indices.R") # make Fig 6

# compare the Z_700 during REPs in the GCM and reanalysis
source('R/compare_Z_during_REPs.R') # make Fig S5

# now make the GCM indices
source('R/make_GCM_indices.R')

# now combine the reanalysis and GCM indices and plot their cdfs
source('R/combine_indices.R') # make Fig 7

# now simulate the historical record for the obs and check the model
source('R/fit_historic_model_and_check.R') # make Figs S8, S9

# fit the model ont he full historic record and compare the wavelet to the observed record
source('R/fit_full_historic_model_and_check.R') # make Fig S8, S9, S10

# now simulate the historical record for the GCM and check the lag1
source('R/sim_historic_model.R') # make Fig 8

# now let's load the future GCM data
# source('R/load_GCM_future.R') # this can take ~ 5 mins to run -- no need to re-run as the processed data is stored in the "Processed_Data" folder

# compute the indices for the future GCM
source('R/make_GCM_indices_future.R')

# combine historic obs indices and future GCM indices
source('R/combine_indices_future.R')

# simulate the future from the GCM
source('R/sim_future_model.R') # make Fig 9

# simulate the future from the GCM but separate the effects of the different predictors
source('R/sim_historic_future_model_sep_effects.R') # make Fig 10

# make several of the supplemental figures
source('R/sup_figs.R') # make Figs S1, S2, S3, S4, S6, S7
