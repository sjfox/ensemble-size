plot_metric_location_performance <- function(folder_path,
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
                                '-model-location_performance.rda')
  
  # browser()
  if(!file.exists(score_save_loc) | refresh_data){
    list.files(folder_path, full.names = T) %>% 
      map(get_model_location_score, 
          test_start_date = test_start_date) %>% 
      bind_rows() %>% 
      mutate(combination_num = as.numeric(str_replace(model, pattern = 'ensemble-', replacement = ''))) %>% 
      left_join(mod_info %>% 
                  distinct(combination_num, k),
                by = 'combination_num') -> location_mod_scores
    save(location_mod_scores, file = score_save_loc)
  } else{
    load(score_save_loc)
  }
  
  load(str_replace(folder_path, 
                   '-analysis',
                   '-model-overall_performance.rda'))
  
  covidHubUtils::hub_locations %>% filter(geo_type == 'state') %>% tail(10)
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
  
  locations_to_exclude <- c('60', '66', '69', '74')
  bind_rows(location_mod_scores %>% 
              filter(combination_num == baseline_ensemble_num, time_period == 'test') %>% 
              mutate(model_name = 'RT Ensemble'),
            location_mod_scores %>% 
              filter(combination_num == baseline_num, time_period == 'test') %>% 
              mutate(model_name = 'Baseline'),
            location_mod_scores %>% 
              filter(combination_num == train_ind_rank_test_perf %>% 
                       filter(k == 5) %>% 
                       pull(combination_num),
                     time_period == 'test') %>% 
              mutate(model_name = 'Individual rank'),
            location_mod_scores %>% 
              filter(combination_num == train_ens_rank_test_perf %>% 
                       filter(k == 5) %>% 
                       pull(combination_num),
                     time_period == 'test') %>% 
              mutate(model_name = 'Ensemble rank')
  ) %>% 
    bind_rows(location_mod_scores %>% 
                group_by(location, time_period, k) %>% 
                summarize(avg_wis = median(avg_wis), .groups = 'drop') %>% 
                filter(k == 5, time_period == 'test') %>% 
                mutate(model_name = 'Median'))  %>% 
    select(model_name, location, avg_wis) %>% 
    left_join(covidHubUtils::hub_locations, by = c('location' = 'fips')) %>%
    filter(!location%in% locations_to_exclude) %>% 
    mutate(model_name = factor(model_name, levels = c('Baseline',
                                                      'RT Ensemble',
                                                      'Median', 
                                                      'Individual rank', 
                                                      'Ensemble rank')),
           abbreviation = forcats::fct_reorder(abbreviation, population)) -> loc_performance
  
  loc_performance %>% 
    ggplot(aes(as.numeric(abbreviation), avg_wis/population, color = model_name)) + 
    geom_line() +
    geom_point() +
    # scale_y_log10() +
    scale_color_brewer(type = 'qual', palette = 2) +
    scale_x_continuous(breaks = as.numeric(unique(loc_performance$abbreviation)),
                       labels = unique(loc_performance$abbreviation)) +
    background_grid(major = 'xy') +
    # scale_x_date(date_labels = '%Y-%b') +
    guides(color = guide_legend(override.aes = list(linewidth = 5))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = NULL, y = 'Relative Average WIS', color = NULL, title = plot_title) 
}
