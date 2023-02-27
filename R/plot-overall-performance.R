plot_metric_overall_performance <- function(folder_path,
                                            mod_info_path,
                                            test_start_date,
                                            refresh_data = FALSE){
  # browser()
  mod_info <- read_csv(mod_info_path)
  
  baseline_ensemble_num <- mod_info %>% 
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
                                '-model-overall_performance.rda')
  # browser()
  if(!file.exists(score_save_loc) | refresh_data){
    list.files(folder_path, full.names = T) %>% 
      map(get_model_overall_score, 
          test_start_date = test_start_date) %>% 
      bind_rows() %>% 
      mutate(combination_num = as.numeric(str_replace(model, pattern = 'ensemble-', replacement = ''))) %>% 
      left_join(mod_info %>% 
                  distinct(combination_num, k),
                by = 'combination_num') -> overall_mod_scores
    save(overall_mod_scores, file = score_save_loc)
  } else{
    load(score_save_loc)
  }
  
  
  overall_mod_scores %>% 
    filter(combination_num %in% 
             get_ind_train_rank_perf(overall_mod_scores, 
                                     baseline_ensemble_num, 
                                     mod_info)) %>% 
    filter(time_period == 'test') -> train_ind_rank_test_perf
  
  overall_mod_scores %>% 
    filter(combination_num %in% 
             get_ens_train_rank_perf(overall_mod_scores, 
                                     baseline_ensemble_num)) %>% 
    filter(time_period == 'test') -> train_ens_rank_test_perf
  
  baseline_performance <- overall_mod_scores %>% 
    filter(combination_num == baseline_num) %>% 
    filter(time_period == 'test') %>% pull(avg_wis)
  ensemble_performance <- overall_mod_scores %>% 
    filter(combination_num == baseline_ensemble_num) %>% 
    filter(time_period == 'test') %>% pull(avg_wis)
  
  
  plot_title <- case_when(grepl(pattern = 'hosp2', x = folder_path, fixed=T) ~ 'COVID-19 admissions',
                          grepl(pattern = 'case', x = folder_path, fixed=T) ~ 'COVID-19 cases',
                          grepl(pattern = 'death', x = folder_path, fixed=T) ~ 'COVID-19 deaths',
                          grepl(pattern = 'flu_hosp', x = folder_path, fixed=T) ~ 'Influenza admissions',
                          T ~ 'Pattern not matched')
  
  # browser()
  rand_select_ensembles <- overall_mod_scores %>% 
    filter(time_period == 'test') %>% 
    group_by(k) %>% 
    summarize(min = min(avg_wis),
              max = max(avg_wis),
              mean = mean(avg_wis),
              median = median(avg_wis), .groups = 'drop')
  rand_select_ensembles %>% 
    select(k, avg_wis = median) %>% 
    mutate(key = 'Median') %>% 
    bind_rows(train_ind_rank_test_perf %>% 
                select(k, avg_wis) %>% 
                mutate(key = 'Individual rank'),
              train_ens_rank_test_perf %>% 
                select(k, avg_wis) %>% 
                mutate(key = 'Ensemble rank')) %>% 
    mutate(key = factor(key, levels = c('Median', 'Individual rank', 'Ensemble rank'))) %>% 
    ggplot(aes(k, avg_wis, color = key)) +
    geom_ribbon(data = rand_select_ensembles,
                aes(x = k, ymin = min, ymax = max), alpha = .1, inherit.aes=F)   +
    geom_hline(yintercept = baseline_performance, lty = 2, color = 'grey30') +
    geom_line() +
    scale_color_manual(values = c('black', '#A16928', '#2887a1')) +
    geom_label_repel(data = tibble(x = Inf,
                                   y = baseline_performance,
                                   label = 'Baseline'),
                     aes(x=x, y=y, label=label),
                     color = 'grey30',
                     nudge_y = baseline_performance*.15,
                     inherit.aes = FALSE) +
    geom_hline(yintercept = ensemble_performance, lty = 2, color = 'grey30') +
    geom_label_repel(data = tibble(x = Inf,
                                   y = ensemble_performance,
                                   label = 'RT Ensemble'),
                     aes(x=x, y=y, label=label),
                     color = 'grey30',
                     nudge_y = ensemble_performance*.15,
                     inherit.aes = FALSE) +
    # scale_y_log10() +
    coord_cartesian(ylim = c(min(rand_select_ensembles$min)*.975, baseline_performance*1.1)) +
    background_grid(major ='xy') +
    labs(x = 'Models included', y = 'Average WIS', title = plot_title, color = NULL) +
    guides(color = guide_legend(override.aes = list(linewidth = 5))) +
    scale_x_continuous(breaks = 1:max(overall_mod_scores$k)) +
    NULL
}
