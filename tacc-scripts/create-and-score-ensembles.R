#####################################################
## Reads in the arguments from command line
## Creates, scores, and saves ensembles
## Parameters required to be specified:

##    analysis_name - name of analysis (hosp1, hosp2, death, case)
##    ensemble_num - number of ensemble in combination


#####################################################
args <- (commandArgs(TRUE)) ## load arguments from R CMD BATCH

if(length(args) > 0)  { ## Then cycle through each element of the list and evaluate the expressions.
  print(paste0('loading in ', args, ' from R CMD BATCH'))
  for(i in 1:length(args)) {
    eval(parse(text=args[[i]]))
  }
}

## Run this line if running locally on Rstudio, without command line parameters
# analysis_name = 'flu_hosp'; ensemble_num = 71

library(tidyverse)
library(covidHubUtils)
library(hubEnsembles)
library(lubridate)
library(here)

if(grepl('spencerfox', Sys.info()['user'])){
  raw_forecast_loc <- here(file.path('raw-data'))
  save_loc <- paste0("../processed-data/", analysis_name,"-analysis")
} else if (grepl('frontera', Sys.info()['nodename'])){
  raw_forecast_loc <- here(file.path('raw-data'))
  save_loc <- paste0("/work2/02958/sjf826/frontera/ensemble-size/", 
                     analysis_name, 
                     "-analysis")
}

source(here('R/data-pull-fxns.R'))
source(here('R/ensemble-scoring-fxns.R'))


# Setup parameters --------------------------------------------------------
model_combination_lookup <- read_csv(here(file.path(raw_forecast_loc,
                                                    paste0(analysis_name, 
                                                           '-model-combination-lookup-table.csv'))))
metric <- case_when(analysis_name == 'death' ~ 'death',
                    analysis_name == 'case' ~ 'case',
                    analysis_name == 'flu_hosp' ~ 'flu_hosp',
                    analysis_name == '2023flu' ~ 'flu_hosp',
                    T ~ 'hosp')
model_name <- paste0("ensemble-", ensemble_num)

## Loads in the truth data
load(here(file.path(raw_forecast_loc, paste0(metric, '-data.rda'))))

## Gets models for ensemble
ensemble_models <- model_combination_lookup %>% 
  filter(combination_num == ensemble_num)

## Get all raw forecast files to summarize
forecast_files <- list.files(here(file.path(raw_forecast_loc, analysis_name)), 
                             full.names = T)


# Read and score ensemble combination -------------------------------------
model_scores <- vector('list', length = length(forecast_files))
for(forecast in forecast_files){
  load(forecast)
  ## Reduce to only ensemble model component forecasts for date
  ## Renames the columns with the aligned numbers
  forecast_data <- forecast_data %>% 
    filter(model %in% ensemble_models$model)
  
  if(nrow(forecast_data) > 0){
    forecast_data <- forecast_data %>% 
      select(-horizon, -forecast_date) %>%
      rename(horizon = relative_horizon, forecast_date = reference_date)
    
    ## Gets the date for forecast from filename
    forecast_date <- get_forecast_date(forecast, 
                                       analysis_name = analysis_name)
    
    ## Creates ensemble for date and scores
    model_scores[[match(forecast, forecast_files)]] <- forecast_data %>% 
      build_quantile_ensemble(method = "median",
                              forecast_date = forecast_date,
                              model_name = model_name) %>% 
      score_forecasts(return_format = "wide",
                      truth = truth_data,
                      metrics = c("wis", 'quantile_coverage'),
                      use_median_as_point = T)  %>% 
      select(model, forecast_date, location, horizon, temporal_resolution, 
             target_variable, target_end_date,
             wis, quantile_coverage_0.5, quantile_coverage_0.9)  
  } else{
    model_scores[[match(forecast, forecast_files)]] <- NULL
  }
} 
model_scores %>% 
  bind_rows() -> model_scores_all

# Save scores --------------------------------------------------------
save(model_scores_all, 
     file = file.path(save_loc, paste0(model_name,'-scores.rda')))
