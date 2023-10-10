## Script that summarizes the ensemble scores for the multiyear analysis
library(tidyverse)
library(here)
get_my_model_score_summaries <- function(file_path, 
                                         target_bounds,
                                         train_seasons = c('2010/2011', '2011/2012', '2012/2013', '2013/2014')){

  read_csv(file_path, col_types = 'ccnncc', show_col_types = FALSE) %>% 
    mutate(season_start_year = substr(season, start = 1, stop = 4)) |> 
    mutate(season_week = ifelse(forecast_week > 39,
                                forecast_week,
                                ifelse(season_start_year == 2014,
                                       forecast_week + 53,
                                       forecast_week + 52))) |> 
    inner_join(target_bounds, by = c('season', 'location', 'target')) %>% 
    filter(season_week >= start_week_seq & season_week <= end_week_seq) |> 
    mutate(time_period = ifelse(season %in% train_seasons, 'Train', 'Test'))-> model_scores_all
  
  
  model_scores_all |> 
    group_by(time_period, model) %>% 
    summarize(score = exp(mean(score)), .groups = 'drop') -> overall_score
  
  
  model_scores_all |> 
    group_by(time_period, model, forecast_week, season) %>% 
    summarize(score = exp(mean(score)), .groups = 'drop') -> score_by_date
  
  model_scores_all |> 
    group_by(time_period, model, target) %>% 
    summarize(score = exp(mean(score)), .groups = 'drop') -> score_by_fcast_horizon
  
  model_scores_all |> 
    group_by(time_period, model, location) %>% 
    summarize(score = exp(mean(score)), .groups = 'drop') -> score_by_location
  
  return(list(overall_score = overall_score,
              score_by_date = score_by_date,
              score_by_fcast_horizon = score_by_fcast_horizon,
              score_by_location = score_by_location
  ))
}


get_multiyear_ind_rank_scores <- function(scores,
                                          mod_info,
                                          target_bounds){
  source('R/multiyear-scoring-fxns.R')
  scores %>% 
    filter(time_period == 'Train', 
           k == 1, 
           !rt_ensemble_mod) %>% 
    arrange(desc(score)) %>% 
    select(-model) %>% 
    left_join(mod_info, by = c('combination_num', 'k')) %>% 
    pull(model) -> ind_model_order_train
  
  ensemble_model_choices <- vector('list', length = length(ind_model_order_train))
  selected_model_combos_ranked <- vector('list', length = length(ind_model_order_train))
  for(i in 1:length(ind_model_order_train)){
    print(paste0('Individual rank ensemble of size ', i))
    ensemble_model_choices[[i]] <- tibble(k=i,
                                          model_names = ind_model_order_train[1:i])
    tibble(model = ind_model_order_train[1:i]) |> 
      get_scored_seasons(raw_forecast_loc = 'raw-data',
                         save_loc = '',
                         model_name = paste0('ind_rank-', i),
                         save_scores = F) -> selected_model_combos_ranked[[i]]
  }  
  
  ensemble_model_choices |> 
    bind_rows() |> 
    write_csv('processed-data/my-ind-rank-ensemble-choices.csv')
  
  selected_model_combos_ranked |> 
    bind_rows() |> 
    write_csv('processed-data/my-ind-rank-ensemble-scores.csv')
  
  return(get_my_model_score_summaries('processed-data/my-ind-rank-ensemble-scores.csv',
                                      target_bounds = target_bounds))
}

get_multiyear_ens_rank_scores <- function(scores,
                                          mod_info,
                                          target_bounds){
  ensemble_rank_scores <- vector('list', length = max(mod_info$k))
  ensemble_model_choices <- vector('list', length = max(mod_info$k))
  
  for(curr_k in 1:max(mod_info$k)){
    print(paste0('Ensemble rank ensemble of size ', curr_k))
    scores |> 
      filter(k == curr_k) |> 
      filter(time_period == 'Train', 
             k == curr_k) |> 
      arrange(desc(score)) %>%
      head(1) |> 
      pull(model) -> best_scoring_ensemble_size_k
    
    
    ensemble_model_choices[[curr_k]] <- tibble(k = curr_k,
                                          ensemble_num = best_scoring_ensemble_size_k)
    
    file_path <- paste0('processed-data/multiyear-analysis/', 
                        best_scoring_ensemble_size_k,
                        '_scores.csv')
    
    ensemble_rank_scores[[curr_k]] <- read_csv(file_path, 
                                               col_types = 'ccnncc', 
                                               show_col_types = FALSE) |> 
      mutate(model = paste0('ens_rank-', curr_k))
  }
  
  ensemble_model_choices |> 
    bind_rows() |> 
    write_csv('processed-data/my-ens-rank-ensemble-choices.csv')
  
  ensemble_rank_scores |> 
    bind_rows() |> 
    write_csv('processed-data/my-ens-rank-ensemble-scores.csv')
  
  return(get_my_model_score_summaries('processed-data/my-ens-rank-ensemble-scores.csv',
                                      target_bounds = target_bounds))
}

combine_my_model_score_summaries <- function(analysis_name,
                                              folder_path,
                                              mod_info_path,
                                              target_bounds){
  # browser()
  ## Get model combination lookup table for metric
  mod_info <- read_csv(mod_info_path)
  
  ## Get model combinations for the baseline and rt ensemble models
  rt_ensemble_num <- mod_info %>% 
    filter(k==max(k)) %>% 
    pull(combination_num) |> unique()
  
  baseline_num <- mod_info %>% 
    filter(k == 1, model == 'ReichLab_kde') %>% 
    pull(combination_num)
  
  ## Pull all the different summary scores for the metric
  list.files(folder_path, full.names = T) %>% 
    map(get_my_model_score_summaries, 
        target_bounds = target_bounds) -> summaries
  
  # save(summaries, file = 'processed-data/temp-multiyear.rda')
  # load(file = 'processed-data/temp-multiyear.rda')
  
  # ## Format all summary scores for different views (overall, by location, by horizion, by date, etc)
  summaries <- transpose(summaries)
  summaries$overall_score |>
    bind_rows() |>
    mutate(combination_num = as.numeric(str_replace(model, pattern = 'ensemble-', replacement = ''))) |> 
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
   
  # ## Get all scores for individual rank ensembles
  individual_rank_scores <- get_multiyear_ind_rank_scores(scores = overall_scores,
                                                           mod_info = mod_info,
                                                           target_bounds = target_bounds)
  
  ensemble_rank_scores <- get_multiyear_ens_rank_scores(scores = overall_scores,
                                                          mod_info = mod_info,
                                                          target_bounds = target_bounds)

  ## Save all components for later use
  save(overall_scores,
       scores_by_date,
       scores_by_fcast_horizon,
       scores_by_location,
       individual_rank_scores,
       ensemble_rank_scores,
       file = paste0('processed-data/', analysis_name, '-score-summaries.rda'))
}

target_bounds <- read_csv('raw-data/all-target-bounds.csv')
colnames(target_bounds) = tolower(colnames(target_bounds))

combine_my_model_score_summaries(analysis_name = 'multiyear',
                                 folder_path = 'processed-data/multiyear-analysis/',
                                 mod_info_path = 'raw-data/multiyear-model-combination-lookup-table.csv',
                                 target_bounds = target_bounds)


