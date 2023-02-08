## Figure helper functions

get_model_overall_score <- function(file_path, 
                                    test_start_date){
  load(file_path)
  if(!exists("model_scores_all")){
    browser()
  }
  model_scores_all %>% 
    filter(location != 'US') %>% 
    mutate(time_period = ifelse(forecast_date < test_start_date, 'train', 'test')) %>% 
    group_by(time_period, model) %>% 
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

plot_metric_overall_performance <- function(folder_path,
                                            mod_info_path,
                                            test_start_date,
                                            refresh_data = FALSE){
  # browser()
  mod_info <- read_csv(mod_info_path)
  
  baseline_ensemble_num <- mod_info %>% 
    filter(model == 'COVIDhub-4_week_ensemble') %>% 
    pull(combination_num)
  baseline_num <- mod_info %>% 
    filter(model == 'COVIDhub-baseline', k == 1) %>% 
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
    coord_cartesian(ylim = c(ensemble_performance*.9, baseline_performance*1.1)) +
    background_grid(major ='xy') +
    labs(x = 'Models included', y = 'Average WIS', title = plot_title, color = NULL) +
    scale_x_continuous(breaks = 1:max(overall_mod_scores$k)) +
    NULL
}
