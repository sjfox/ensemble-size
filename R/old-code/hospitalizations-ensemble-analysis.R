library(tidyverse)
library(covidHubUtils)
library(hubEnsembles)
library(lubridate)


# Model list and date setup -----------------------------------------------
forecast_target <- 'hosp'
models <- c('BPagano-RtDriven', 'CMU-TimeSeries', 
            'COVIDhub-baseline', 'CU-select', 
            'CUB_PopCouncil-SLSTM', 'GT-DeepCOVID', 
            'Karlen-pypm', 'MOBS-GLEAM_COVID', 
            'MUNI-ARIMA', 'PSI-DICE', 
            'USC-SI_kJalpha', 'COVIDhub-4_week_ensemble')
dates <- as.character(seq.Date(from = ymd('2022-02-07'), to = ymd('2022-11-07'), by = 7))


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
if(file.exists(truth_data_filename)){
  load(truth_data_filename)
}
# Create all look up table for model combinations -------------------------
get_model_combinations <- function(k, models){
  n <- length(models)  
  combn(n, m = k)  %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(k = k,
           combo_num = seq_along(k)) %>% 
    gather(model_num, model, -k, -combo_num) %>% 
    arrange(combo_num) %>% 
    select(k, combo_num, model)
}

tibble(k = 1:length(models),
       model_combinations = map(k, get_model_combinations, models = models)) %>% 
  select(-k) %>% 
  unnest(model_combinations) %>% 
  mutate(model = models[model]) %>% 
  group_by(k,combo_num) %>% 
  filter(k ==1 | !('COVIDhub-4_week_ensemble' %in% model)) %>% 
  ungroup() %>% 
  nest(data = 'model') %>% 
  mutate(combination_num = seq_along(combo_num)) %>% 
  select(combination_num, k, data) %>% 
  unnest(data) -> model_combination_lookup

## Save lookup table for future reference if needed
write_csv(model_combination_lookup, 
          file = paste0(ensemble_base_folder, 'model-combination-lookup-table.csv'))
# model_combination_lookup <- read_csv('processed-data/death-model-combination-lookup-table.csv')
model_names <- paste0('ensemble-', unique(model_combination_lookup$combination_num))

## Stop here unless rerunning!!!


# Download and save all model forecasts for dates -------------------------
for(cur_date in dates){
  forecast_data <- load_forecasts(
    models = models,
    targets = c(paste(1:30, "day ahead inc hosp")),
    dates = ymd(cur_date),
    date_window_size = 2,
    locations = NULL,
    hub = c("US"),
    types = "quantile",
    verbose=F)
  
  forecast_data %>% 
    align_forecasts() %>% 
    filter(relative_horizon %in% 1:28) -> forecast_data
  
  save(forecast_data, file = paste0(raw_forecast_loc, cur_date, '_all-individual-forecasts.rda'))
}

# Save truth data ---------------------------------------------------------
truth_data <- load_truth(
  truth_source = "HealthData", 
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
    group_by(model, time_period) %>% 
    summarize(n = n(),
              avg_wis = mean(wis), .groups = 'drop')  
}

summary_scores %>% 
  bind_rows() -> summary_scores
save(summary_scores, 
     file = paste0(ensemble_base_folder, 'summary-model-scores.rda'))


# Pull performance of best picked models ----------------------------------
load(paste0(ensemble_base_folder, 'summary-model-scores.rda'))


# Pull ensemble data ------------------------------------------------------
ens_forecast_data <- load_forecasts(
  models = 'COVIDhub-4_week_ensemble',
  targets = c(paste(c(7, 14, 21, 28), "day ahead inc hosp")),
  dates = dates,
  date_window_size = 1,
  locations = NULL,
  types = "quantile")

score_forecasts(
  forecasts = ens_forecast_data,
  return_format = "wide",
  truth = truth_data,
  metrics = "wis",
  use_median_as_point = T) -> ensemble_forecast_score

ensemble_wis <- ensemble_forecast_score %>% 
  filter(location != 'US') %>% 
  summarize(avg_wis = mean(wis)) %>% 
  pull(avg_wis)
## Combine all scores into data frame for plotting
summary_scores %>% 
  mutate(combination_num = str_replace(model, pattern = 'ensemble-', replacement = '')) %>% 
  filter(combination_num == (model_combination_lookup %>% 
                               filter(k == 1, model == 'COVIDhub-baseline') %>% pull(combination_num))) %>% 
  summarize(avg_wis = sum(n*avg_wis)/sum(n)) %>% 
  pull(avg_wis) -> baseline_wis



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
  ggplot(aes(k, value, color = key)) + 
  geom_line() +
  labs(x = 'Number of models',
       y = 'Average WIS') +
  scale_x_continuous(breaks = 0:11) +
  geom_hline(yintercept = baseline_wis, lty = 2) +
  annotate('text', x = 9, y = baseline_wis, label = 'Baseline WIS', vjust=-0.7) +
  geom_hline(yintercept = ensemble_wis, lty = 2, color = 'darkgrey') +
  annotate('text', x = 2, y = ensemble_wis, label = 'Ensemble WIS', vjust=-0.7, color = 'darkgrey')


# Time-series plot of performance -----------------------------------------
## Get average score by date for all ensembles
date_scores <- vector('list', length = length(model_names))
for(curr_model in model_names){
  load(paste0(ensemble_base_folder, 'model-scores/', curr_model, '_model-scores.rda'))
  
  date_scores[[match(curr_model, model_names)]] <- mod_df %>% 
    filter(location != 'US') %>% 
    group_by(model, forecast_date) %>% 
    summarize(n = n(), avg_wis = mean(wis), .groups = 'drop')  
}

date_scores %>% 
  bind_rows() %>% 
  mutate(combo_num = as.integer(str_replace(model, 'ensemble-', ''))) %>% 
  left_join(model_combination_lookup %>% 
              distinct(combination_num,k), by = c('combo_num' = 'combination_num')) %>% 
  group_by(k, forecast_date) %>% 
  summarize(min = min(avg_wis),
            max = max(avg_wis),
            mean = mean(avg_wis),
            median = median(avg_wis), .groups = 'drop') -> ts_summary_scores


truth_data %>% 
  filter(location == 'US', target_end_date %in% unique(mod_df$target_end_date))->temp %>% 
  ggplot(aes(target_end_date, value)) + 
    geom_point() +
    labs(y = 'Hospitalizations', x = NULL) +
    scale_x_date(limits = c(min(ts_summary_scores$forecast_date), 
                            max(ts_summary_scores$forecast_date))) ->truth_ts_plot
truth_ts_plot

baseline_ts <- date_scores %>% 
  bind_rows() %>% 
  mutate(combination_num = str_replace(model, pattern = 'ensemble-', replacement = '')) %>% 
  filter(combination_num == (model_combination_lookup %>% 
                               filter(k == 1, model == 'COVIDhub-baseline') %>% pull(combination_num)))
  
full_ensemble_ts <- ensemble_forecast_score %>% 
  filter(location != 'US') %>% 
  group_by(forecast_date) %>% 
  summarize(avg_wis = mean(wis)) 

## Combine all scores into data frame for plotting
summary_scores %>% 
  mutate(combination_num = str_replace(model, pattern = 'ensemble-', replacement = '')) %>% 
  filter(combination_num == (model_combination_lookup %>% 
                               filter(k == 1, model == 'COVIDhub-baseline') %>% pull(combination_num))) %>% 
  summarize(avg_wis = sum(n*avg_wis)/sum(n))

ts_summary_scores %>% 
  filter(k %in% c(2, 5, 11)) %>% 
  select(-mean) %>% 
  gather(key, value, min:median) %>% 
  ggplot(aes(forecast_date, value, color = as.factor(k), fill = as.factor(k))) +
    # geom_ribbon(aes(ymin = min, ymax = max), alpha = .1, color = NA) +
    geom_line() +
    theme(legend.position = 'bottom') +
    geom_line(data = baseline_ts %>% 
                expand_grid(key = c('max','median','min')) %>% 
                mutate(avg_wis = ifelse(key!='max' & avg_wis>50, NA, avg_wis)), aes(forecast_date, avg_wis), inherit.aes=F, lty = 2, color = 'black') +
    geom_line(data = full_ensemble_ts, aes(forecast_date, avg_wis), inherit.aes=F, lty = 2, color = 'darkgrey') +
    facet_wrap(~key, nrow = 3, scales='free_y') +
    labs(color = 'Ensemble number') -> ensemble_ts_plot
ensemble_ts_plot

library(cowplot)
plot_grid(truth_ts_plot,
          ensemble_ts_plot,
          rel_heights = c(1, 3), nrow = 2, align = 'v')  




