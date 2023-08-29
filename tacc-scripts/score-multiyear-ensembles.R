#####################################################
## Reads in the arguments from command line
## Aggregates, Scores, and saves the ensemble
## Parameters required to be specified:

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
# ensemble_num = 65

library(tidyverse)
library(here)
analysis_name <- 'multiyear'

if(grepl('spencerfox', Sys.info()['user'])){
  raw_forecast_loc <- here(file.path('raw-data'))
  save_loc <- here(paste0("processed-data/", analysis_name,"-analysis"))
} else if (grepl('frontera', Sys.info()['nodename'])){
  raw_forecast_loc <- here(file.path('raw-data'))
  save_loc <- paste0("/work2/02958/sjf826/frontera/ensemble-size/", 
                     analysis_name, 
                     "-analysis")
}
source(here("R/multiyear-scoring-fxns.R"))

## Initialize specific parameters
model_combination_lookup <- read_csv(here(file.path(raw_forecast_loc,
                                                    paste0(analysis_name, 
                                                           '-model-combination-lookup-table.csv'))))

task_models <- model_combination_lookup %>% 
  filter(combination_num == ensemble_num)

scored_seasons <- get_scored_seasons(task_models,
                                     raw_forecast_loc,
                                     model_name = paste0("ensemble-", ensemble_num),
                                     save_loc)
