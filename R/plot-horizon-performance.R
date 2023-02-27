plot_metric_horizon_performance <- function(folder_path,
                                       mod_info_path,
                                       test_start_date,
                                       refresh_data = FALSE){
  mod_info <- read_csv(mod_info_path)
  
  
  ## Get baseline and ensemble combination numbers
  baseline_ensemble_num<- mod_info %>% 
    filter(model == 'COVIDhub-4_week_ensemble' |
             model == 'Flusight-ensemble') %>% 
    pull(combination_num)
  baseline_num <- mod_info %>% 
    filter(model == 'COVIDhub-baseline' |
             model == 'Flusight-baseline', 
           k == 1) %>% 
    pull(combination_num)
  
  score_save_loc <- str_replace(folder_path, 
                                '-analysis',
                                '-model-horizon_performance.rda')
  
  # browser()
  if(!file.exists(score_save_loc) | refresh_data){
    list.files(folder_path, full.names = T) %>% 
      map(get_model_horizon_score, 
          test_start_date = test_start_date) %>% 
      bind_rows() %>% 
      mutate(combination_num = as.numeric(str_replace(model, pattern = 'ensemble-', replacement = ''))) %>% 
      left_join(mod_info %>% 
                  distinct(combination_num, k),
                by = 'combination_num') -> horizon_mod_scores
    save(horizon_mod_scores, file = score_save_loc)
  } else{
    load(score_save_loc)
  }
  
  load(str_replace(folder_path, 
                   '-analysis',
                   '-model-overall_performance.rda'))
  
  
  
  ## Get trained time period ranks
  overall_mod_scores %>% 
    filter(combination_num %in% 
             get_ind_train_rank_perf(overall_mod_scores, 
                                     baseline_ensemble_num, 
                                     mod_info)) %>% 
    filter(time_period == 'train') -> train_ind_rank_test_perf
  
  overall_mod_scores %>% 
    filter(combination_num %in% 
             get_ens_train_rank_perf(overall_mod_scores, 
                                     baseline_ensemble_num)) %>% 
    filter(time_period == 'train') -> train_ens_rank_test_perf
  
  plot_title <- case_when(grepl(pattern = 'hosp2', x = folder_path, fixed=T) ~ 'COVID-19 admissions',
                          grepl(pattern = 'case', x = folder_path, fixed=T) ~ 'COVID-19 cases',
                          grepl(pattern = 'death', x = folder_path, fixed=T) ~ 'COVID-19 deaths',
                          grepl(pattern = 'flu_hosp', x = folder_path, fixed=T) ~ 'Influenza admissions',
                          T ~ 'Pattern not matched')
  
  plot_xaxis <- case_when(grepl(pattern = 'hosp2', x = folder_path, fixed=T) ~ 'Forecast horizon (days)',
                          grepl(pattern = 'case', x = folder_path, fixed=T) ~ 'Forecast horizon (weeks)',
                          grepl(pattern = 'death', x = folder_path, fixed=T) ~ 'Forecast horizon (weeks)',
                          grepl(pattern = 'flu_hosp', x = folder_path, fixed=T) ~ 'Forecast horizon (weeks)',
                          T ~ 'Pattern not matched')
  
  
  bind_rows(horizon_mod_scores %>% 
              filter(combination_num == baseline_ensemble_num, time_period == 'test') %>% 
              mutate(model_name = 'RT Ensemble'),
            horizon_mod_scores %>% 
              filter(combination_num == baseline_num, time_period == 'test') %>% 
              mutate(model_name = 'Baseline'),
            horizon_mod_scores %>% 
              filter(combination_num == train_ind_rank_test_perf %>% 
                       filter(k == 5) %>% 
                       pull(combination_num),
                     time_period == 'test') %>% 
              mutate(model_name = 'Individual rank'),
            horizon_mod_scores %>% 
              filter(combination_num == train_ens_rank_test_perf %>% 
                       filter(k == 5) %>% 
                       pull(combination_num),
                     time_period == 'test') %>% 
              mutate(model_name = 'Ensemble rank')
  ) %>% 
    select(model_name, horizon, median=avg_wis) %>% 
    bind_rows(horizon_mod_scores %>% 
                group_by(horizon, time_period, k) %>% 
                summarize(min = min(avg_wis),
                          max = max(avg_wis),
                          mean = mean(avg_wis),
                          median = median(avg_wis), .groups = 'drop') %>% 
                filter(k == 5, time_period == 'test') %>% 
                mutate(model_name = 'Median'))  %>% 
    # left_join(horizon_mod_scores %>% 
    #           filter(combination_num == baseline_ensemble_num) %>% 
    #           mutate(model_name = 'RT Ensemble') %>% 
    #           select(forecast_date, avg_wis)) %>% 
    mutate(model_name = factor(model_name, levels = c('Baseline',
                                                      'RT Ensemble',
                                                      'Median', 
                                                      'Individual rank', 
                                                      'Ensemble rank'))) -> hor_performance
  
  hor_performance %>% 
    ggplot(aes(horizon, median, color = model_name)) + 
    geom_line() +
    # scale_y_log10() +
    scale_color_brewer(type = 'qual', palette = 2) +
    background_grid(major = 'xy') +
    # scale_x_date(date_labels = '%Y-%b') +
    guides(color = guide_legend(override.aes = list(linewidth = 5))) +
    labs(x = plot_xaxis, y = 'Average WIS', color = NULL, title = plot_title) 
}
