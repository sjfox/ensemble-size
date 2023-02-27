## Figure helper functions

get_model_overall_score <- function(file_path, 
                                    test_start_date){
  load(file_path)
  if(!exists("model_scores_all")){
    browser()
  }
  model_scores_all %>% 
    filter(location %in% (covidHubUtils::hub_locations  %>% 
                            filter(geo_type == 'state') %>% pull(fips))) %>% 
    filter(location != 'US') %>% 
    mutate(time_period = ifelse(forecast_date < test_start_date, 'train', 'test')) %>% 
    group_by(time_period, model) %>% 
    summarize(avg_wis = mean(wis),
              q50 = mean(quantile_coverage_0.5),
              q90 = mean(quantile_coverage_0.9), 
              .groups = 'drop')
}
get_model_ts_score <- function(file_path, 
                                     test_start_date){
  load(file_path)
  if(!exists("model_scores_all")){
    browser()
  }
  model_scores_all %>% 
    filter(location %in% (covidHubUtils::hub_locations  %>% 
                            filter(geo_type == 'state') %>% pull(fips))) %>% 
    filter(location != 'US') %>% 
    group_by(forecast_date, model) %>% 
    summarize(avg_wis = mean(wis),
              q50 = mean(quantile_coverage_0.5),
              q90 = mean(quantile_coverage_0.9), 
              .groups = 'drop') %>% 
    mutate(time_period = ifelse(forecast_date < test_start_date, 'train', 'test')) 
}

get_model_horizon_score <- function(file_path, 
                                    test_start_date){
  load(file_path)
  if(!exists("model_scores_all")){
    browser()
  }
  model_scores_all %>% 
    filter(location %in% (covidHubUtils::hub_locations  %>% 
                            filter(geo_type == 'state') %>% pull(fips))) %>% 
    filter(location != 'US') %>% 
    mutate(time_period = ifelse(forecast_date < test_start_date, 'train', 'test')) %>%  
    group_by(time_period, model, horizon) %>% 
    summarize(avg_wis = mean(wis),
              q50 = mean(quantile_coverage_0.5),
              q90 = mean(quantile_coverage_0.9), 
              .groups = 'drop') 
    
}

get_model_location_score <- function(file_path, 
                                    test_start_date){
  load(file_path)
  if(!exists("model_scores_all")){
    browser()
  }
  model_scores_all %>% 
    filter(location %in% (covidHubUtils::hub_locations  %>% 
                            filter(geo_type == 'state') %>% pull(fips))) %>% 
    mutate(time_period = ifelse(forecast_date < test_start_date, 'train', 'test')) %>%  
    group_by(time_period, model, location) %>% 
    summarize(avg_wis = mean(wis),
              q50 = mean(quantile_coverage_0.5),
              q90 = mean(quantile_coverage_0.9), 
              .groups = 'drop') 
  
}

get_ind_train_rank_perf <- function(scores,
                                    baseline_ensemble_num,
                                    mod_info){
  # browser()
  scores %>% 
    filter(time_period == 'train', 
           k == 1, 
           combination_num != baseline_ensemble_num) %>% 
    arrange(avg_wis) %>% 
    select(-model) %>% 
    left_join(mod_info, by = c('combination_num', 'k')) %>% 
    pull(model) -> ind_model_order_train
  
  selected_model_combos_ranked <- vector('numeric', length = length(ind_model_order_train))
  for(i in 1:length(ind_model_order_train)){
    selected_model_combos_ranked[i] <- mod_info %>% 
      filter(model %in% ind_model_order_train[1:i], k == i) %>% 
      count(combination_num) %>% 
      filter(n == i) %>% 
      pull(combination_num)
  }  
  return(selected_model_combos_ranked)
}

get_ens_train_rank_perf <- function(scores, baseline_ensemble_num){
  # browser()
  scores %>% 
    filter(time_period == 'train',
           combination_num != baseline_ensemble_num) %>% 
    group_by(k) %>% 
    filter(avg_wis == min(avg_wis)) %>% 
    arrange(k) %>% 
    pull(combination_num)
}




