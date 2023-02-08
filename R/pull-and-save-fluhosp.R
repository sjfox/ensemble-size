### Pulls hospitalization data for ensemble-size analysis hosp 2 time period
library(tidyverse)
library(covidHubUtils)
library(hubEnsembles)
library(lubridate)
source('R/data-pull-fxns.R')

# Model analysis setup -----------------------------------------------
truth_target <- 'flu_hosp'
analysis_name <- 'flu_hosp'
models <- c('Flusight-baseline','PSI-DICE', 
            'SigSci-TSENS', 'GT-FluFNP', 
            'CMU-TimeSeries', 'SigSci-CREG', 
            'SGroup-RandomForest', 'MOBS-GLEAM_FLUH',
            'Flusight-ensemble')



forecast_dates <- as.character(c(seq.Date(from = ymd('2022-01-10'), 
                                        to = ymd('"2022-06-20"'), by = 7),
                                 seq.Date(from = ymd('2022-10-17'), 
                                          to = ymd('"2023-01-02"'), by = 7)))
forecast_target_names <- c(paste(1:4, "wk ahead inc flu hosp"))
forecast_rel_horizons <- c(1:4)
hub <- 'FluSight'

# Create all folders for analysis -----------------------------------------
truth_data_filename <-  paste0('raw-data/', truth_target, '-data.rda')
raw_forecast_loc <- paste0('raw-data/', analysis_name, '/')
ensemble_base_folder <- paste0('processed-data/', analysis_name, '-analysis/')

create_folders(raw_forecast_loc, 
               ensemble_base_folder)

# Download and save all model forecasts for dates -------------------------
download_analysis_forecasts(forecast_dates = forecast_dates, 
                            models = NULL,
                            forecast_target_names = forecast_target_names,
                            forecast_rel_horizons = forecast_rel_horizons,
                            raw_forecast_loc = raw_forecast_loc,
                            forecast_hub = hub,
                            source = 'local_hub_repo',
                            hub_repo_path = '~/projects/Flusight-forecast-data/',
                            data_processed_subpath = 'data-forecasts/',
                            as_of = NULL)



# Save truth data (if needed) ---------------------------------------------
if(!file.exists(truth_data_filename)){
  get_truth_data(truth_target, 
                 truth_data_filename)
  
}

# Create look up table for all model combinations -------------------------
get_all_model_combinations(models = models,
                           analysis_name = analysis_name)

# temp <- read_csv('raw-data/flu_hosp-model-combination-lookup-table.csv')
# temp %>% 
#   distinct(combination_num) %>% 
#   nrow()
# sum(choose((length(models)-1), 1:(length(models)-1))) + 1
# temp %>% 
#   filter(model == 'Flusight-ensemble')
