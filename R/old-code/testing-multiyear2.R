library(FluSight) 
library(tidyverse)

source("R/stack_forecasts.R")


CURRENT_SEASON <- "2010/2011"


## get list of component models
folder_list <- list.files(path = 'raw-data/component-models', full.names = T)
folder_list <- folder_list[!grepl('*.csv', folder_list)]
get_model_name <- function(folder_path){
  model_name <- str_split(folder_path, pattern = '/')[[1]]
  model_name[length(model_name)]
}


model_names <- tibble(base_dir = folder_list,
                      model_name = folder_list %>% map(get_model_name) %>% unlist())

this_week <- 'EW01-2011'
files_to_stack <- paste0(model_names$base_dir, "/", 
                         this_week, "-", model_names$model_name, ".csv")
file_df <- tibble(file = files_to_stack, 
                  model_id = model_names$model_name)
stacked_entry <- stack_forecasts(file_df)

stacked_entry %>% 
  group_by(location, target) %>% 
  summarize(tot_prob = sum(value)) %>% 
  filter(grepl('wk', target)) %>% 
  filter(round(tot_prob,digits = 2)!=1)
read_entry(files_to_stack[[1]]) %>% nrow()


for(j in 1:length(weight_files)){
  stacking_weights <- read.csv(paste0("weights/", weight_files[j]), 
                               stringsAsFactors=FALSE)
  stacked_name <- sub(pattern = ".csv", replacement = "", weight_files[j])
  dir.create(file.path("model-forecasts", "cv-ensemble-models", stacked_name), 
             showWarnings = FALSE)
  


  
  
  
    ## loop through each season and each season-week to make stacked forecasts
  for(i in 1:length(seasons)){
    loso_season =  seasons[i]
    wt_subset <- filter(stacking_weights, season==loso_season) %>%
      dplyr::select(-season)
    
    ## identify the "EWXX-YYYY" combos for files given season
    first_year <- substr(loso_season, 0, 4)
    first_year_season_weeks <- if(first_year==2014) {43:53} else {43:52}
    week_names <- c(
      paste0("EW", first_year_season_weeks, "-", first_year),
      paste0("EW", formatC(1:18, width=2, flag=0), "-", as.numeric(first_year)+1)
    )
    
    foreach(k=1:length(week_names)) %dopar% {
      ## for(k in 1:length(week_names)) {
      this_week <- week_names[k]
      message(paste(stacked_name, "::", this_week, "::", Sys.time()))
      ## stack models, save ensemble file
      files_to_stack <- paste0(
        "model-forecasts/component-models/",
        model_names$model.dir, "/",
        this_week, "-", model_names$model.dir, ".csv"
      )
      file_df <- data.frame(
        file = files_to_stack, 
        model_id = model_names$model.id,
        stringsAsFactors = FALSE)
      stacked_entry <- stack_forecasts(file_df, wt_subset)
      stacked_file_name <- paste0(
        "model-forecasts/cv-ensemble-models/",
        stacked_name, "/", this_week, "-", stacked_name, ".csv"
      )
      write.csv(stacked_entry, file=stacked_file_name, 
                row.names = FALSE, quote = FALSE)
    }
  }
}
