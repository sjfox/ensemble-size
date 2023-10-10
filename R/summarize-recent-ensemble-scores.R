## Script that summarizes the ensemble scores for all of the analyses
library(tidyverse)

get_model_score_summaries <- function(file_path, 
                                        test_start_date){
  load(file_path)
  if(!exists("model_scores_all")){
    browser()
  }
  model_scores_all %>% 
    filter(location %in% (covidHubUtils::hub_locations  %>% 
                            filter(geo_type == 'state') %>% pull(fips))) %>% 
    filter(location != 'US') %>% 
    mutate(time_period = ifelse(forecast_date < test_start_date, 'train', 'test')) -> model_scores_all
  
  model_scores_all |> 
    group_by(time_period, model) %>% 
    summarize(avg_wis = mean(wis),
              q50 = mean(quantile_coverage_0.5),
              q90 = mean(quantile_coverage_0.9), 
              .groups = 'drop') -> overall_score
  
  model_scores_all |> 
    group_by(time_period, forecast_date, model) %>% 
    summarize(avg_wis = mean(wis),
              q50 = mean(quantile_coverage_0.5),
              q90 = mean(quantile_coverage_0.9), 
              .groups = 'drop') -> score_by_date
  model_scores_all |> 
    group_by(time_period, model, horizon) %>% 
    summarize(avg_wis = mean(wis),
              q50 = mean(quantile_coverage_0.5),
              q90 = mean(quantile_coverage_0.9), 
              .groups = 'drop') -> score_by_fcast_horizon
  
  model_scores_all |>
    group_by(time_period, model, location) %>% 
    summarize(avg_wis = mean(wis),
              q50 = mean(quantile_coverage_0.5),
              q90 = mean(quantile_coverage_0.9), 
              .groups = 'drop')  -> score_by_location
  
  return(list(overall_score = overall_score,
              score_by_date = score_by_date,
              score_by_fcast_horizon = score_by_fcast_horizon,
              score_by_location = score_by_location
              ))
}

get_ind_train_rank_perf <- function(scores, 
                                    mod_info){
  # browser()
  scores %>% 
    filter(time_period == 'train', 
           k == 1, 
           !rt_ensemble_mod) %>% 
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

get_ens_train_rank_perf <- function(scores){
  # browser()
  scores %>% 
    filter(time_period == 'train',
           !rt_ensemble_mod) %>% 
    group_by(k) %>% 
    filter(avg_wis == min(avg_wis)) %>% 
    arrange(k) %>% 
    pull(combination_num)
}

combine_model_score_summaries <- function(analysis_name,
                                          folder_path,
                                          mod_info_path,
                                          test_start_date){
  ## Get model combination lookup table for metric
  mod_info <- read_csv(mod_info_path)
  
  ## Get model combinations for the baseline and rt ensemble models
  rt_ensemble_num <- mod_info %>% 
    filter(model == 'COVIDhub-4_week_ensemble' |
             model == 'Flusight-ensemble') %>% 
    pull(combination_num)
  
  baseline_num <- mod_info %>% 
    filter(model == 'COVIDhub-baseline' |
             model == 'Flusight-baseline', 
           k == 1) %>% 
    pull(combination_num)
  
  ## Pull all the different summary scores for the metric
  list.files(folder_path, full.names = T) %>% 
    map(get_model_score_summaries, 
        test_start_date = test_start_date) -> summaries
  
  ## Format all summary scores for different views (overall, by location, by horizion, by date, etc)
  summaries <- transpose(summaries)
  summaries$overall_score |> 
    bind_rows() |> 
    mutate(combination_num = as.numeric(str_replace(model, pattern = 'ensemble-', replacement = ''))) %>% 
    left_join(mod_info %>% 
                distinct(combination_num, k),
              by = 'combination_num') |> 
    mutate(baseline_mod = combination_num == baseline_num,
           rt_ensemble_mod = combination_num == rt_ensemble_num) -> overall_scores
  
  summaries$score_by_date |> 
    bind_rows() |> 
    mutate(combination_num = as.numeric(str_replace(model, pattern = 'ensemble-', replacement = ''))) %>% 
    left_join(mod_info %>% 
                distinct(combination_num, k),
              by = 'combination_num') |> 
    mutate(baseline_mod = combination_num == baseline_num,
           rt_ensemble_mod = combination_num == rt_ensemble_num) -> scores_by_date
  
  summaries$score_by_fcast_horizon |> 
    bind_rows() |> 
    mutate(combination_num = as.numeric(str_replace(model, pattern = 'ensemble-', replacement = ''))) %>% 
    left_join(mod_info %>% 
                distinct(combination_num, k),
              by = 'combination_num') |> 
    mutate(baseline_mod = combination_num == baseline_num,
           rt_ensemble_mod = combination_num == rt_ensemble_num) -> scores_by_fcast_horizon
  
  summaries$score_by_location |> 
    bind_rows() |> 
    mutate(combination_num = as.numeric(str_replace(model, pattern = 'ensemble-', replacement = ''))) %>% 
    left_join(mod_info %>% 
                distinct(combination_num, k),
              by = 'combination_num') |> 
    mutate(baseline_mod = combination_num == baseline_num,
           rt_ensemble_mod = combination_num == rt_ensemble_num) -> scores_by_location
  
  ## Get all scores for individual rank ensembles
  ind_rank_ensembles <- get_ind_train_rank_perf(scores =overall_scores,
                                                mod_info = mod_info)
  individual_rank_scores <- list(overall = overall_scores |> 
                                   filter(combination_num %in% ind_rank_ensembles),
                                 date = scores_by_date |> 
                                   filter(combination_num %in% ind_rank_ensembles),
                                 fcast_horizon = scores_by_fcast_horizon |> 
                                   filter(combination_num %in% ind_rank_ensembles),
                                 location = scores_by_location |> 
                                   filter(combination_num %in% ind_rank_ensembles))
  
  ## Get the ensemble rank summarized scores
  ens_rank_ensembles <- get_ens_train_rank_perf(scores = overall_scores)
  ensemble_rank_scores <- list(overall = overall_scores |> 
                                   filter(combination_num %in% ens_rank_ensembles),
                                 date = scores_by_date |> 
                                   filter(combination_num %in% ens_rank_ensembles),
                                 fcast_horizon = scores_by_fcast_horizon |> 
                                   filter(combination_num %in% ens_rank_ensembles),
                                 location = scores_by_location |> 
                                   filter(combination_num %in% ens_rank_ensembles))
  
  ## Save all components for later use
  save(overall_scores, 
       scores_by_date, 
       scores_by_fcast_horizon, 
       scores_by_location, 
       individual_rank_scores,
       ensemble_rank_scores,
       file = paste0('processed-data/', analysis_name, '-score-summaries.rda'))
}

combine_model_score_summaries(analysis_name = 'case',
                              folder_path = 'processed-data/case-analysis',
                              mod_info_path = 'raw-data/case-model-combination-lookup-table.csv',
                              test_start_date = '2021-11-15')

combine_model_score_summaries(analysis_name = 'hosp2',
                              folder_path = 'processed-data/hosp2-analysis',
                              mod_info_path = 'raw-data/hosp2-model-combination-lookup-table.csv',
                              test_start_date = '2022-06-01')

combine_model_score_summaries(analysis_name = 'death',
                              folder_path = 'processed-data/death-analysis',
                              mod_info_path = 'raw-data/death-model-combination-lookup-table.csv',
                              test_start_date = '2021-11-15')

combine_model_score_summaries(analysis_name = 'flu_hosp',
                              folder_path = 'processed-data/flu_hosp-analysis',
                              mod_info_path = 'raw-data/flu_hosp-model-combination-lookup-table.csv',
                              test_start_date = '2022-10-01')



