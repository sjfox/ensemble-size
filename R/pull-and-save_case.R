### Pulls hospitalization data for ensemble-size analysis hosp 2 time period
library(tidyverse)
library(covidHubUtils)
library(hubEnsembles)
library(lubridate)
source('R/data-pull-fxns.R')

# Model analysis setup -----------------------------------------------
truth_target <- 'case'
analysis_name <- 'case'
models <- c('BPagano-RtDriven', 'CovidAnalytics-DELPHI', 
            'COVIDhub-baseline', 'CU-select', 
            'JHUAPL-Bucky', 'RobertWalraven-ESG', 
            'USC-SI_kJalpha', 'COVIDhub-4_week_ensemble')

forecast_dates <- as.character(seq.Date(from = ymd('2020-11-02'), 
                                        to = ymd('2022-07-25'), by = 7))
forecast_target_names <- c(paste(1:4, "wk ahead inc case"))
forecast_rel_horizons <- c(1:4)
hub <- 'US'

# Create all folders for analysis -----------------------------------------
truth_data_filename <-  paste0('raw-data/', truth_target, '-data.rda')
raw_forecast_loc <- paste0('raw-data/', analysis_name, '/')
ensemble_base_folder <- paste0('processed-data/', analysis_name, '-analysis/')

create_folders(raw_forecast_loc, 
               ensemble_base_folder)

# Download and save all model forecasts for dates -------------------------
download_analysis_forecasts(forecast_dates = forecast_dates, 
                            models = models,
                            forecast_target_names = forecast_target_names,
                            forecast_rel_horizons = forecast_rel_horizons,
                            raw_forecast_loc = raw_forecast_loc,
                            forecast_hub = hub)


# Save truth data (if needed) ---------------------------------------------
if(!file.exists(truth_data_filename)){
  get_truth_data(truth_target, 
                 truth_data_filename)
  
}

# Create look up table for all model combinations -------------------------
get_all_model_combinations(models = models,
                           ensemble_base_folder = ensemble_base_folder)
