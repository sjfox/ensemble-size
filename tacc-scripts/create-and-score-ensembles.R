#####################################################
## Reads in the arguments from command line
## Creates, scores, and saves ensembles
## Parameters required to be specified:

##    analysis_name - name of analysis (hosp1, hosp2, death, case)
##    forecast_date - date (YYYY-MM-DD)
##    metric - name of metric (case, death, hosp)

#####################################################
args <- (commandArgs(TRUE)) ## load arguments from R CMD BATCH

if(length(args)>0)  { ## Then cycle through each element of the list and evaluate the expressions.
  print(paste0('loading in ', args, ' from R CMD BATCH'))
  for(i in 1:length(args)) {
    eval(parse(text=args[[i]]))
  }
}

## Run this line if running locally on Rstudio, without command line parameters
# analysis_name = 'hosp1'; forecast_date = '2020-12-07'; metric = 'hosp'

library(tidyverse)
library(covidHubUtils)
library(hubEnsembles)
library(lubridate)
library(here)

if(grepl('spencerfox', Sys.info()['user'])){
  save_loc <- paste0("processed-data/", analysis_name,"-analysis/")
} else if (grepl('frontera', Sys.info()['nodename'])){
  save_loc <- paste0("/work2/02958/sjf826/frontera/ensemble-size/", 
                     analysis_name, 
                     "-analysis/")
}

source(here('R/data-pull-fxns.R'))
source(here('R/ensemble-scoring-fxns.R'))

## Setup parameters
raw_forecast_file <- here(file.path('raw-data',
                                    analysis_name,
                                    paste0(forecast_date, '_all-individual-forecasts.rda')))
model_combination_lookup <- read_csv(here(paste0("processed-data/", 
                                                 analysis_name,
                                                 "-analysis/model-combination-lookup-table.csv")))

## Loads in the truth data
load(here(paste0('raw-data/', metric, '-data.rda')))

##Load in the raw forecast data
load(raw_forecast_file)

# Create all ensembles for date ----------------------------------------------------
ensemble_forecasts <- get_all_ensemble_forecasts(forecast_data,
                                                 forecast_date,
                                                 model_combination_lookup)

# Score all ensembles -----------------------------------------------------
score_forecasts(
  forecasts = ensemble_forecasts,
  return_format = "wide",
  truth = truth_data,
  metrics = "wis",
  use_median_as_point = T) -> ensemble_scores

# Save created files ------------------------------------------------------
## Ensemble forecasts
## Save all ensemble forecasts
save(ensemble_forecasts, 
     file = file.path(save_loc, 
                      'ensembles', 
                      paste0(forecast_date, '_ensembles.rda')))
save(ensemble_scores, 
     file = file.path(save_loc, 
                      'ensembles-scores', 
                      paste0(forecast_date, '_ensemble-scores.rda')))

