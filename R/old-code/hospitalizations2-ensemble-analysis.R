library(tidyverse)
library(covidHubUtils)
library(hubEnsembles)
library(lubridate)


# Model list and date setup -----------------------------------------------
forecast_target <- 'hosp2'
models <- c('COVIDhub-baseline', 'Karlen-pypm', 'JHUAPL-Bucky',
            'CU-select', 'LANL-GrowthRate', 'USC-SI_kJalpha', 
            'JHU_IDD-CovidSP', 'MOBS-GLEAM_COVID',
            'Covid19Sim-Simulator')
dates <- as.character(seq.Date(from = ymd('2020-12-07'), to = ymd('2021-07-26'), by = 7))


# Create all folders for analysis -----------------------------------------
ensemble_base_folder <- paste0('processed-data/',forecast_target, '-analysis/')
truth_data_filename <- paste0('raw-data/jhu-', forecast_target, '-all.rda')
raw_forecast_loc <- paste0('raw-data/historic-forecasts/', forecast_target, '/')
if(!dir.exists(ensemble_base_folder)){
  dir.create(ensemble_base_folder)
}
if(!dir.exists(paste0(ensemble_base_folder, 'ensembles'))){
  dir.create(paste0(ensemble_base_folder, 'ensembles'))
}
if(!dir.exists(paste0(ensemble_base_folder, 'ensemble-scores'))){
  dir.create(paste0(ensemble_base_folder, 'ensemble-scores'))
}
if(!dir.exists(paste0(ensemble_base_folder, 'model-scores'))){
  dir.create(paste0(ensemble_base_folder, 'model-scores'))
}
if(!dir.exists(raw_forecast_loc)){
  dir.create(raw_forecast_loc)
}

# Create all look up table for model combinations -------------------------
get_model_combinations <- function(k, models){
  n <- length(models)  
  combn(n, m = k)  %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(k = k,
           comb_num = seq_along(k)) %>% 
    gather(model_num, model, -k, -comb_num) %>% 
    arrange(comb_num) %>% 
    select(k, comb_num, model)
}

tibble(k = 1:length(models),
       model_combinations = map(k, get_model_combinations, models = models)) %>% 
  select(-k) %>% 
  unnest(model_combinations) %>% 
  nest(data = 'model') %>% 
  mutate(combination_num = seq_along(comb_num)) %>% 
  select(combination_num, k, data) %>% 
  unnest(data) %>% 
  mutate(model = models[model]) -> model_combination_lookup

## Save lookup table for future reference if needed
write_csv(model_combination_lookup, 
          file = paste0(ensemble_base_folder, 'model-combination-lookup-table.csv'))
# model_combination_lookup <- read_csv('processed-data/death-model-combination-lookup-table.csv')

## Stop here unless rerunning!!!


# Download and save all model forecasts for dates -------------------------
for(cur_date in dates){
  forecast_data <- load_forecasts(
    models = models,
    targets = c(paste(c(7, 14, 21, 28), "day ahead inc hosp")),
    dates = ymd(cur_date),
    date_window_size = 1,
    locations = NULL,
    types = "quantile")
  save(forecast_data, file = paste0(raw_forecast_loc, cur_date, '_all-individual-forecasts.rda'))
}

# Save truth data ---------------------------------------------------------
truth_data <- load_truth(
  truth_source = "JHU", 
  target_variable = "inc hosp", 
  locations = NULL
)
save(truth_data, file = truth_data_filename)
load(truth_data_filename)



# Create and save ensembles -----------------------------------------------
get_quantile_forecast_model_combination <- function(combination_num,
                                                    models_included, 
                                                    forecast_df,
                                                    forecast_date){
  build_quantile_ensemble(forecast_df %>% 
                            filter(model %in% models_included$model), 
                          method = "median",
                          forecast_date = forecast_date,
                          model_name = paste0("ensemble-", combination_num),
                          location_data = hub_locations)
}


for(forecast_date in dates){
  print(forecast_date)
  load(paste0(raw_forecast_loc, forecast_date, '_all-individual-forecasts.rda'))
  model_combination_lookup %>% 
    nest(data = c('k', 'model')) %>% 
    mutate(ensemble_forecast = map2(combination_num,
                                    data,
                                    .f = get_quantile_forecast_model_combination,
                                    forecast_df = forecast_data,
                                    forecast_date = forecast_date)) %>% 
    select(-data) %>% 
    unnest(ensemble_forecast) -> ensemble_forecasts
  save(ensemble_forecasts, 
       file = paste0(ensemble_base_folder, 'ensembles/', forecast_date, '_ensembles.rda'))
  rm(ensemble_forecasts)
  gc()
} 

# Score all ensembles -----------------------------------------------------
for(forecast_date in dates){
  if(match(forecast_date, dates) %% 10 == 0){
    print(paste0('Scoring date ', match(forecast_date, dates), ' of ', length(dates)))
  }
  load(paste0(ensemble_base_folder, 'ensembles/', forecast_date, '_ensembles.rda'))
  score_forecasts(
    forecasts = ensemble_forecasts,
    return_format = "wide",
    truth = truth_data,
    metrics = "wis",
    use_median_as_point = T
  ) -> ensemble_scores
  save(ensemble_scores, 
       file = paste0(ensemble_base_folder, 'ensemble-scores/', forecast_date, '_ensemble-scores.rda'))
} 



# Combine the scores into model specific dataframes -----------------------
model_names <- paste0('ensemble-', unique(model_combination_lookup$combination_num))
for(curr_model in model_names){
  model_scores <- vector('list', length = length(dates))
  for(forecast_date in dates){
    load(paste0(ensemble_base_folder, 'ensemble-scores/', forecast_date, '_ensemble-scores.rda'))
    model_scores[[match(forecast_date, dates)]] <- ensemble_scores %>% 
      filter(model == curr_model)
  } 
  model_scores %>% 
    bind_rows() -> mod_df
  
  save(mod_df, 
       file = paste0(ensemble_base_folder, 'model-scores/', curr_model, '_model-scores.rda'))
}


# Create summary scores for time periods each model -----------------------
summary_scores <- vector('list', length = length(model_names))
for(curr_model in model_names){
  load(paste0(ensemble_base_folder, 'model-scores/', curr_model, '_model-scores.rda'))
  
  summary_scores[[match(curr_model, model_names)]] <- mod_df %>% 
    filter(location != 'US') %>% 
    mutate(time_period = ifelse(forecast_date < '2022-05-01', 'train', 'test')) %>% 
    group_by(model, time_period) %>% 
    summarize(n = n(),
              avg_wis = mean(wis), .groups = 'drop')  
}

summary_scores %>% 
  bind_rows() -> summary_scores
save(summary_scores, 
     file = paste0(ensemble_base_folder, 'summary-model-scores.rda'))


# Pull performance of best picked models ----------------------------------
## Arrange by individual scores
summary_scores %>% 
  filter(time_period == 'train') %>% 
  mutate(combination_num = str_replace(model, pattern = 'ensemble-', replacement = '')) %>% 
  select(-model) %>% 
  inner_join(model_combination_lookup %>% 
               filter(k==1) %>% 
               mutate(combination_num = as.character(combination_num)), 
             by = 'combination_num') %>% 
  arrange(avg_wis) %>% 
  pull(model) -> model_performance_order

## Get the model combination number for the groupings
selected_model_combos_ranked <- vector('numeric', length = length(model_performance_order))
for(i in 1:length(model_performance_order)){
  selected_model_combos_ranked[i] <- model_combination_lookup %>% 
    filter(model %in% model_performance_order[1:i], k == i) %>% 
    count(combination_num) %>% 
    filter(n == i) %>% 
    pull(combination_num)
}  

summary_scores %>% 
  mutate(combo_num = as.integer(str_replace(model, 'ensemble-', ''))) %>% 
  left_join(model_combination_lookup %>% 
              distinct(combination_num,k), by = c('combo_num' = 'combination_num')) %>% 
  filter(time_period == 'train') %>% 
  group_by(k) %>% 
  filter(avg_wis == min(avg_wis)) %>% 
  pull(combo_num) -> selected_model_combos_ensemble

## Combine all scores into data frame for plotting
summary_scores %>%
  mutate(combo_num = as.integer(str_replace(model, 'ensemble-', ''))) %>% 
  left_join(model_combination_lookup %>% 
              distinct(combination_num,k), by = c('combo_num' = 'combination_num')) %>% 
  # filter(time_period == 'test') %>% 
  group_by(k) %>% 
  summarize(min = min(avg_wis),
            max = max(avg_wis),
            mean = mean(avg_wis),
            median = median(avg_wis)) %>% 
  gather(key, value, -k) %>% 
  # bind_rows(summary_scores %>% 
  #             filter(time_period == 'test') %>% 
  #             mutate(combination_num = str_replace(model, pattern = 'ensemble-', replacement = '')) %>% 
  #             select(-model) %>% 
  #             left_join(model_combination_lookup %>% 
  #                         mutate(combination_num = as.character(combination_num)) %>% 
  #                         count(combination_num) %>% 
  #                         rename(n_models = n)) %>% 
  #             filter(combination_num %in% selected_model_combos_ranked) %>% 
  #             mutate(key = 'rankorder') %>% 
  #             select(key, k=n_models, value = avg_wis),
#           summary_scores %>% 
#             filter(time_period == 'test') %>% 
#             mutate(combination_num = str_replace(model, pattern = 'ensemble-', replacement = '')) %>% 
#             select(-model) %>% 
#             left_join(model_combination_lookup %>% 
#                         mutate(combination_num = as.character(combination_num)) %>% 
#                         count(combination_num) %>% 
#                         rename(n_models = n)) %>% 
#             filter(combination_num %in% selected_model_combos_ensemble) %>% 
#             mutate(key = 'ensemble') %>% 
#             select(key, k=n_models, value = avg_wis)) %>% 
ggplot(aes(k, value, color = key)) + 
  geom_line() +
  labs(x = 'Number of models',
       y = 'Average WIS') +
  scale_x_continuous(breaks = 0:11)


# Create overall figure ---------------------------------------------------

load(paste0(ensemble_base_folder, 'summary-model-scores.rda'))

summary_scores %>%
  bind_rows() %>% 
  mutate(combo_num = as.integer(str_replace(model, 'ensemble-', ''))) %>% 
  left_join(model_combination_lookup %>% 
              distinct(combination_num,k), by = c('combo_num' = 'combination_num')) %>% 
  group_by(k) %>% 
  summarize(min = min(avg_wis),
            max = max(avg_wis),
            mean = mean(avg_wis),
            median = median(avg_wis)) %>% 
  gather(key, value, -k) %>% 
  ggplot(aes(k, value, color = key)) + 
  geom_line() +
  labs(x = 'Number of models',
       y = 'Average WIS') +
  scale_x_continuous(breaks = 0:10)





