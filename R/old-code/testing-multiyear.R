
library(tidyverse)
# devtools::install_github("jarad/FluSight")
library(FluSight) 
library(MMWRweek)

get_single_forecast_entry <- function(file_path){
  # browser()
  entry <- read_entry(file_path)
  file_parts <- strsplit(file_path, split = "/")[[1]]
  file_only <- file_parts[length(file_parts)]
  entry %>% mutate(year = as.numeric(substr(file_only, 6, 9)),
                   model_name = substr(file_only, 11, gregexpr("\\.", file_only)[[1]]-1))
}

score_multi_entry <- function(entry, truth){
  # browser()
  names(entry) <- tolower(names(entry))
  names(truth) <- tolower(names(truth))
  if (!("forecast_week" %in% names(entry))) 
    stop("Column forecast_week needed in entry - \n         use read_entry() with your submission CSV")
  seasonal <- entry %>% 
    filter(type == "Bin", 
           target %in% c("Season onset", 
                         "Season peak week", 
                         "Season peak percentage", 
                         "Season peak rate")) %>% 
    right_join(truth, by = c("location", "target", "bin_start_incl")) %>% 
    filter(target %in% c("Season onset", "Season peak week", 
                         "Season peak percentage", "Season peak rate")) %>% 
    select(-forecast_week.y) %>% 
    rename(forecast_week = forecast_week.x)
  weekly <- entry %>% 
    filter(type == "Bin", target %in% c("1 wk ahead", 
                                        "2 wk ahead", 
                                        "3 wk ahead", 
                                        "4 wk ahead")) %>% 
    right_join(truth, by = c("location", "target", 
                             "bin_start_incl", "forecast_week")) %>% 
    filter(target %in% c("1 wk ahead", 
                         "2 wk ahead", 
                         "3 wk ahead", 
                         "4 wk ahead"))
  scores <- bind_rows(seasonal, 
                      weekly) %>% 
    group_by(model_name,
             year,
             location, 
             target, 
             forecast_week) %>% 
    summarize(score = log(sum(value))) %>% 
    ungroup() %>% 
    mutate(score = ifelse(score < -10 | is.na(score), -10, score))
  return(scores)
}


files <- list.files('raw-data/FluSightNetwork-cdc-flusight-ensemble-e7d16c9/model-forecasts/component-models/ReichLab_kde/', 
                    full.names = T)
files <- files[grepl('2010', files)]

files %>% 
  map(get_single_forecast_entry) %>% 
  bind_rows() -> forecast_df

truth_df <- create_truth(year = 2010)

score_multi_entry(forecast_df, truth_df) %>% 
  as_tibble() -> temp
temp


# ew_to_seasonweek <- function(EW, year, season_start_week=30){
#   require(MMWRweek)
#   num_days <- ifelse(MMWRweek::MMWRweek(as.Date(paste0(as.character(year),"-12-28")))[2] == 53, 53, 52)
#   return(ifelse(EW > season_start_week, EW - season_start_week, (EW + num_days) - season_start_week))
# }
all_target_bounds <- read_csv("raw-data/FluSightNetwork-cdc-flusight-ensemble-e7d16c9/writing/comparison/data/all-target-bounds.csv")
temp %>% 
  dplyr::left_join(all_target_bounds %>%
                     filter(Season == '2010/2011'),
                   by = c('target'="Target",
                          'location' = "Location")) %>%
  # mutate(model_week = ew_to_seasonweek(forecast_week, 2010)) %>% 
  dplyr::filter(forecast_week <= start_week, forecast_week >= end_week) %>% 
  group_by(year) %>% 
  summarize(mean(exp(score)))


scores %>% 
  
  dplyr::filter(`Model Week` >= start_week_seq, `Model Week` <= end_week_seq)



# Choosing the models for ensembling --------------------------------------
temp <- read_csv('raw-data/FluSightNetwork-cdc-flusight-ensemble-e7d16c9/model-forecasts/component-models/complete-modelids.csv')
temp %>% 
  count(season)

temp %>% filter(season == '2010/2011')
models_to_use <- c('CU-EKF_SEIRS', 'CU-EKF_SIRS', 'Delphi-DeltaDensity1', 
                   'ReichLab-KCDE', 'LANL-DBMplus', 'ReichLab-SARIMA1', 
                   'CU-BMA', 'Delphi-BasisRegression', 'Delphi-EmpiricalFuture', 
                   'ReichLab-KDE', 'Delphi-EmpiricalBayes1', 'UTAustin-edm')
length(models_to_use)
sum(choose(13, 1:13))

temp %>% 
  filter(`model-id`%in% models_to_use) %>% distinct(`model-id`)
  count(season)

temp %>% pull(`model-id`) %>% unique()


folder_list <- list.files(path = 'raw-data/component-models/', full.names = T)

folder_list <- folder_list[!grepl('*.csv', folder_list)]

get_forecast_ct <- function(folder_path){
  # browser()
  model_dir <- str_split(folder_path, pattern = '/')[[1]]
  model_dir <- model_dir[length(model_dir)]
  
  file_list <- list.files(folder_path)
  file_list <- file_list[!grepl('2017', file_list)]
  file_list <- file_list[!grepl('2018', file_list)]
  file_list <- file_list[!grepl('2019', file_list)]
  file_list <- file_list[!grepl('.txt', file_list)]
  
  tibble(model = model_dir,
         n = length(file_list))
}

folder_list %>% map(get_forecast_ct) %>% bind_rows() -> df
df %>% filter(n!=212)
