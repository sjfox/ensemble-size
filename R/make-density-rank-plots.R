library(tidyverse)
library(cowplot)
library(covidHubUtils)
library(ggridges)
theme_set(theme_cowplot())

get_clean_model_scores <- function(file_path, 
                                          test_start_date = NULL,
                                          target_bounds = target_bounds,
                                          train_seasons = c('2010/2011', '2011/2012', '2012/2013', '2013/2014')){
  if(!is.null(test_start_date)){
    ## Recent ensembles
    load(file_path)
    if(!exists("model_scores_all")){
      browser()
    }
    model_scores_all %>% 
      filter(location %in% (covidHubUtils::hub_locations  %>% 
                              filter(geo_type == 'state') %>% pull(fips))) %>% 
      filter(location != 'US') %>% 
      filter(forecast_date >= test_start_date)
  } else {
    ## Multiyear ensembles
    # browser()
    read_csv(file_path, col_types = 'ccnncc', show_col_types = FALSE) %>% 
      inner_join(target_bounds, by = c('season', 'location', 'target')) %>% 
      filter(ifelse(start_week>=38, 
                    (forecast_week>=start_week | forecast_week <= end_week),
                    (forecast_week>=start_week & forecast_week <= end_week))) |> 
      filter(!(season%in% train_seasons))
  }
}


plot_ranked_density <- function(ensemble_scores_folder,
                                 summary_score_file,
                                 model_info_path,
                                 plot_title, 
                                 test_start_date = NULL,
                                 target_bounds = NULL){
  
  # browser()
  ## Figure out which models we need to pull forecasts from

  ## Individual rank forecasts
  if(!is.null(test_start_date)){
    load(summary_score_file)
    
    ind_rank_combo <- individual_rank_scores$overall |> 
      filter(k == 5,time_period == 'test') |> pull(combination_num)
    
    ## Ensemble rank forecasts
    ens_rank_combo <- ensemble_rank_scores$overall |> 
      filter(k == 5,time_period == 'test') |> pull(combination_num) 
    ## Get individual models and RT ensemble with others
    mod_info <- read_csv(model_info_path)
    combos_needed <- mod_info |> 
      filter(k==1) |> 
      select(-k) |> 
      bind_rows(tibble(combination_num = c(ens_rank_combo, ind_rank_combo),
                       model = c('Ensemble-Rank', 'Individual-Rank'))) |> 
      rename(model_name = model) |> 
      mutate(model = paste0('ensemble-', combination_num))
    
    models_needed <- paste0(combos_needed$model, '-scores.rda')
    
    all_potential_files <- list.files(ensemble_scores_folder, full.names = T)
    files_needed <- c()
    for(model in models_needed){
      files_needed = c(files_needed, all_potential_files[grepl(model, all_potential_files, fixed = T)])
    }
    files_needed |> 
      map(get_clean_model_scores, test_start_date = test_start_date) |> 
      bind_rows() -> all_model_scores
    
    all_model_scores |> 
      left_join(combos_needed,
                by = 'model') |> 
      group_by(location, forecast_date, horizon) |> 
      mutate(n_models = n()) %>%
      # arrange(score) %>%
      mutate(model_rank = rank(wis, ties.method= "min"), 
             rank_percentile = 1-model_rank/n_models) |> 
      ungroup() -> model_ranks
    
  } else{
    mod_info <- read_csv(model_info_path)
    
    combos_needed <- mod_info |> 
      filter(k==1) |> 
      select(-k) |> 
      rename(model_name = model) |> 
      mutate(model = paste0('ensemble-', combination_num)) |> 
      bind_rows(mod_info |> 
                  filter(k==max(k)) |> 
                  distinct(combination_num) |> 
                  mutate(model_name = 'RT-Ensemble') |> 
                  mutate(model = paste0('ensemble-', combination_num)))
    
    models_needed <- paste0(combos_needed$model, '_scores.csv')
    
    all_potential_files <- list.files(ensemble_scores_folder, full.names = T)
    files_needed <- c()
    for(model in models_needed){
      files_needed = c(files_needed, all_potential_files[grepl(model, all_potential_files, fixed = T)])
    }
    files_needed |> 
      map(get_clean_model_scores, 
          target_bounds = target_bounds) |> 
      bind_rows() |> 
      bind_rows(get_clean_model_scores('processed-data/my-ens-rank-ensemble-scores.csv',
                                           target_bounds = target_bounds) |> 
                      filter(model == 'ens_rank-5'),
                get_clean_model_scores('processed-data/my-ind-rank-ensemble-scores.csv',
                                       target_bounds = target_bounds) |> 
                  filter(model == 'ind_rank-5')) -> all_model_scores
    
    
  combos_needed |> 
    bind_rows(tibble(combination_num = c(-1,-2),
                     model_name = c('Ensemble-Rank', 'Individual-Rank'),
                     model = c('ens_rank-5', 'ind_rank-5'))) ->combos_needed
  all_model_scores |> 
    mutate(score = exp(score)) |> 
    left_join(combos_needed,
              by = 'model') |> 
    group_by(location, forecast_week, season, target) |> #mutate(model_rank = ) -> temp
    mutate(n_models = n()) %>%
    # arrange(score) %>%
    mutate(model_rank = rank(1/score, ties.method= "min"), 
           rank_percentile = 1-model_rank/n_models) |> 
    ungroup() -> model_ranks
    
  }
  
  model_ranks |> 
    group_by(model_name) |> 
    summarize(avg_rank = median(rank_percentile)) -> model_ordering
  
  model_ranks |> 
    left_join(model_ordering, by = 'model_name') |> 
    mutate(model_name = fct_reorder(model_name, avg_rank)) |> 
    ggplot(aes(y = model_name, x = rank_percentile, fill = factor(stat(quantile)))) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                        quantiles = 4, quantile_lines = TRUE,
                        bandwidth = 0.05) +
    scale_fill_viridis_d() +
    scale_x_continuous(name="standardized rank", 
                       limits=c(0,1)) +
    labs(fill = 'Quartile', y = NULL, title = plot_title)
}

plot_ranked_density(ensemble_scores_folder = 'processed-data/case-analysis',
                    summary_score_file = 'processed-data/case-score-summaries.rda',
                    model_info_path = 'raw-data/case-model-combination-lookup-table.csv',
                    test_start_date = '2021-11-15',
                    plot_title = 'COVID-19 cases') -> case_rank_plot
save_plot('figs/case_rank_plot.png', case_rank_plot, base_height = 6, base_asp = 1.4, bg = 'white')

plot_ranked_density(ensemble_scores_folder = 'processed-data/hosp2-analysis',
                    summary_score_file = 'processed-data/hosp2-score-summaries.rda',
                    model_info_path = 'raw-data/hosp2-model-combination-lookup-table.csv',
                    test_start_date = '2022-06-01',
                    plot_title = 'COVID-19 admissions')-> hosp_rank_plot
save_plot('figs/hosp_rank_plot.png', hosp_rank_plot, base_height = 6, base_asp = 1.4, bg = 'white')

plot_ranked_density(ensemble_scores_folder = 'processed-data/death-analysis',
                    summary_score_file = 'processed-data/death-score-summaries.rda',
                    model_info_path = 'raw-data/death-model-combination-lookup-table.csv',
                    test_start_date = '2021-11-15',
                    plot_title = 'COVID-19 mortality') -> death_rank_plot
save_plot('figs/death_rank_plot.png', death_rank_plot, base_height = 6, base_asp = 1.4, bg = 'white')


plot_ranked_density(ensemble_scores_folder = 'processed-data/flu_hosp-analysis',
                    summary_score_file = 'processed-data/flu_hosp-score-summaries.rda',
                    model_info_path = 'raw-data/flu_hosp-model-combination-lookup-table.csv',
                    test_start_date = '2022-10-01',
                    plot_title = 'Influenza admissions') -> fluhosp_rank_plot
save_plot('figs/fluhosp_rank_plot.png', fluhosp_rank_plot, base_height = 6, base_asp = 1.4, bg = 'white')



target_bounds <- read_csv('raw-data/all-target-bounds.csv')
colnames(target_bounds) = tolower(colnames(target_bounds))

plot_ranked_density(ensemble_scores_folder = 'processed-data/multiyear-analysis',
                    summary_score_file = 'processed-data/multiyear-score-summaries.rda',
                    model_info_path = 'raw-data/multiyear-model-combination-lookup-table.csv',
                    test_start_date = NULL,
                    target_bounds = target_bounds,
                    plot_title = 'ILI %') -> multiyear_rank_plot
save_plot('figs/multiyear_rank_plot.png', multiyear_rank_plot, base_height = 7, base_asp = 1.4, bg = 'white')








