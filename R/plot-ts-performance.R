plot_metric_ts_performance <- function(folder_path,
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
                                '-model-ts_performance.rda')
  
  # browser()
  if(!file.exists(score_save_loc) | refresh_data){
    list.files(folder_path, full.names = T) %>% 
      map(get_model_ts_score, 
          test_start_date = test_start_date) %>% 
      bind_rows() %>% 
      mutate(combination_num = as.numeric(str_replace(model, pattern = 'ensemble-', replacement = ''))) %>% 
      left_join(mod_info %>% 
                  distinct(combination_num, k),
                by = 'combination_num') -> ts_mod_scores
    save(ts_mod_scores, file = score_save_loc)
  } else{
    load(score_save_loc)
  }
  # browser()
  if(grepl('hosp2', mod_info_path) | grepl('hosp1', mod_info_path)){
    load('raw-data/hosp-data.rda')  
  } else{
    load(str_replace(mod_info_path, 
                     '-model-combination-lookup-table.csv',
                     '-data.rda'))
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
  
  plot_yaxis <- case_when(grepl(pattern = 'hosp2', x = folder_path, fixed=T) ~ 'COVID-19 admissions/10k',
                          grepl(pattern = 'case', x = folder_path, fixed=T) ~ 'COVID-19 cases/10k',
                          grepl(pattern = 'death', x = folder_path, fixed=T) ~ 'COVID-19 deaths/10k',
                          grepl(pattern = 'flu_hosp', x = folder_path, fixed=T) ~ 'Flu admissions/10k',
                          T ~ 'Pattern not matched')

  

  # browser()
  bind_rows(ts_mod_scores %>% 
              filter(combination_num == baseline_ensemble_num) %>% 
              mutate(model_name = 'RT Ensemble'),
            ts_mod_scores %>% 
              filter(combination_num == baseline_num) %>% 
              mutate(model_name = 'Baseline'),
            ts_mod_scores %>% 
              filter(combination_num == train_ind_rank_test_perf %>% 
                       filter(k == 5) %>% 
                       pull(combination_num),
                     time_period == 'test') %>% 
              mutate(model_name = 'Individual rank'),
            ts_mod_scores %>% 
              filter(combination_num == train_ens_rank_test_perf %>% 
                       filter(k == 5) %>% 
                       pull(combination_num),
                     time_period == 'test') %>% 
              mutate(model_name = 'Ensemble rank')
  ) %>% 
    select(model_name, forecast_date, time_period, median=avg_wis) %>% 
    bind_rows(ts_mod_scores %>% 
                group_by(forecast_date, time_period, k) %>% 
                summarize(min = min(avg_wis),
                          max = max(avg_wis),
                          mean = mean(avg_wis),
                          median = median(avg_wis), .groups = 'drop') %>% 
                filter(k == 5) %>% 
                mutate(model_name = 'Median'))  %>% 
    # left_join(ts_mod_scores %>% 
    #           filter(combination_num == baseline_ensemble_num) %>% 
    #           mutate(model_name = 'RT Ensemble') %>% 
    #           select(forecast_date, avg_wis)) %>% 
    mutate(model_name = factor(model_name, levels = c('Baseline',
                                                      'RT Ensemble',
                                                      'Median', 
                                                      'Individual rank', 
                                                      'Ensemble rank'))) -> rel_performance
  
  date_start <- min(rel_performance$forecast_date)
  date_end <- max(rel_performance$forecast_date)
  train_rect <- tibble(ymin = 0, 
                       ymax = Inf, 
                       xmin = structure(-Inf, class = "Date"), 
                       xmax = as.Date(test_start_date))
  
  true_test_start_date <- rel_performance %>% 
    filter(time_period=='test') %>% 
    filter(forecast_date == min(forecast_date)) %>% pull(forecast_date)
  
  truth_data %>% 
    filter(location == 'US') %>% 
    ggplot(aes(target_end_date, value/10000)) +
    geom_vline(xintercept = true_test_start_date, lty = 2, alpha = .5)+
    geom_rect(data = train_rect,
              aes(ymin=ymin, ymax=ymax,xmin=xmin,xmax=xmax), 
              alpha = .05, fill = 'black', color = NA, inherit.aes=F) +
    geom_line() +
    coord_cartesian(xlim = c(date_start, date_end)) +
    background_grid(major = 'xy') +
    scale_x_date(date_labels = '%Y-%b') +
    labs(x = NULL, y = plot_yaxis) -> metric_plot

  rel_performance %>% 
    filter(time_period == 'test') %>%
    ggplot(aes(forecast_date, median, color = model_name)) + 
    geom_vline(xintercept = true_test_start_date, lty = 2, alpha = .5)+
    geom_rect(data = train_rect,
              aes(ymin=ymin, ymax=ymax, xmin=xmin,xmax=xmax), 
              alpha = .05, fill = 'black', color = NA, inherit.aes=F) +
      geom_line() +
      geom_line(data = rel_performance %>%
                  filter(time_period == 'train', 
                         model_name %in% c('Median', 'RT Ensemble', 'Baseline'))) +
      scale_y_log10() +
      scale_color_brewer(type = 'qual', palette = 2) +
      # scale_color_manual(values = c('black', '#A16928', '#2887a1')) +
      geom_hline(yintercept = 0, lty = 2) +
      background_grid(major = 'xy') +
      scale_x_date(date_labels = '%Y-%b') +
      guides(color = guide_legend(override.aes = list(linewidth = 5))) +
      labs(x = NULL, y = 'Relative WIS', color = NULL) +
      theme(legend.position = 'bottom') -> performance_plot

  
  
  plot_grid(metric_plot,
            performance_plot + theme(legend.position = 'none'),
            nrow = 2,
            align = 'hv') %>% 
    plot_grid(get_legend(performance_plot + 
                           theme(legend.justification = c(0.5,0.5))) ,
              nrow = 2,
              rel_heights = c(1,.08))
  
  
}
