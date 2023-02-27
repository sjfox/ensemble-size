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
model_name <- paste0("ensemble-", ensemble_num)
seasons <- c("2010/2011", "2011/2012", "2012/2013", "2013/2014", "2014/2015", "2015/2016", "2016/2017")

scored_seasons <- vector('list', length(seasons))
for(i in 1:length(seasons)){
  loso_season =  seasons[i]

  ## identify the "EWXX-YYYY" combos for files given season
  first_year <- substr(loso_season, 0, 4)
  first_year_season_weeks <- if(first_year==2014) {43:53} else {43:52}
  week_names <- c(
    paste0("EW", first_year_season_weeks, "-", first_year),
    paste0("EW", formatC(1:18, width=2, flag=0), "-", as.numeric(first_year)+1)
  )
  
  ## Manage truth data for scoring
  if(!dir.exists(here('raw-data/multiyear-truth'))){
    dir.create(here('raw-data/multiyear-truth'))
  }
  truth_file <- here(paste0('raw-data/multiyear-truth/', first_year, '-ili.rda'))
  if(file.exists(truth_file)){
    load(truth_file)
  }else{
    truth_df <- FluSight::create_truth(year = as.numeric(first_year))
    save(truth_df, file = truth_file)
  }
  
  ensemble_forecasts <- vector('list', length = length(week_names))
  for(k in 1:length(week_names)){
    files_to_stack <- here(paste0(raw_forecast_loc, 
                                  "/component-models/", 
                                  task_models$model, '/',
                                  week_names[k], "-", 
                                  task_models$model, ".csv"))
    stacked_entry <- stack_forecasts(files_to_stack)
    ensemble_forecasts[[k]] <- stacked_entry
  }
  scored_seasons[[i]] <- score_multi_entry(ensemble_forecasts %>% 
                                             bind_rows(), 
                                           truth_df) 
}


if(!dir.exists(save_loc)){
  dir.create(save_loc)
}
scored_seasons %>% 
  bind_rows(.id = 'id') %>% 
  mutate(season = seasons[as.numeric(id)]) %>% 
  select(-id) %>% 
  mutate(model = model_name) %>%  
  write_csv(paste0(save_loc, '/',model_name,'_scores.csv'))
