## Pull statistics for manuscript
library(tidyverse)

tibble(files = list.files('raw-data/', pattern = 'model-combination-lookup-table.csv', full.names = T),
       num_combinations = list.files('raw-data/', pattern = 'model-combination-lookup-table.csv', full.names = T) |> 
         map(read_csv) |> 
         map(.f = function(x) {x |> pull(combination_num) |> max()}) |> 
         unlist())

sum(choose(7,1:7))


## Get the case decline in performance when including more models
load(paste0('processed-data/case-score-summaries.rda'))
overall_scores |> 
  filter(time_period == 'test',
         !rt_ensemble_mod) |>
  group_by(k) |>
  summarize(score = mean(avg_wis)) |>
  filter(k == 4 | k == max(k)) |>
  mutate(k = ifelse(k == 4, 'start', 'finish')) |>
  spread(k, score) |>
  mutate(wane_percent = (finish - start)/start)

overall_scores |> 
  filter(time_period == 'test',
         !rt_ensemble_mod) |>
  group_by(k) |>
  summarize(score = min(avg_wis)) |>
  filter(k == 4 | k == max(k)) |>
  mutate(k = ifelse(k == 4, 'start', 'finish')) |>
  spread(k, score) |>
  mutate(wane_percent = (finish - start)/start)

# Pull statistics about relative to R of forecasts --------------------
pull_rt_comparison_stats <- function(analysis_name, score_col_name){
  # browser()
  load(paste0('processed-data/', analysis_name, '-score-summaries.rda'))
  
  ## Get rt ensemble value 
  overall_scores |> 
    filter(rt_ensemble_mod, tolower(time_period) == 'test') |> 
    pull({{score_col_name}}) -> rt_ensemble_val
  
  # browser()
  if(analysis_name == 'multiyear'){
    ensemble_rank_overall <- ensemble_rank_scores$overall_score
    individual_rank_overall <- individual_rank_scores$overall_score
    
    rt_ensemble_val <- 1/rt_ensemble_val
    
    overall_scores |>
      filter(tolower(time_period) == 'test',
             !rt_ensemble_mod) |>
      group_by(k) |>
      summarize(min_score = min(1/{{score_col_name}})/rt_ensemble_val,
                mean_score = mean(1/{{score_col_name}})/rt_ensemble_val,
                max_score = max(1/{{score_col_name}})/rt_ensemble_val) |>
      mutate(ensemble_type = 'Random') |> 
      bind_rows(ensemble_rank_overall |> 
                  filter(tolower(time_period) == 'test') |> 
                  mutate(mean_score = 1 / {{score_col_name}}/rt_ensemble_val,
                         min_score = NA,
                         max_score = NA,
                         ensemble_type = 'Ensemble rank') |> 
                  mutate(k=as.numeric(str_replace(model, 'ens_rank-', replacement = ''))) |> 
                  select(-time_period, -model, -score),
                individual_rank_overall |> 
                  filter(tolower(time_period) == 'test') |> 
                  mutate(mean_score = 1 / {{score_col_name}}/rt_ensemble_val,
                         min_score = NA,
                         max_score = NA,
                         ensemble_type = 'Individual rank') |> 
                  mutate(k=as.numeric(str_replace(model, 'ind_rank-', replacement = ''))) |> 
                  select(-time_period, -model, -score)) |> 
      mutate(analysis = analysis_name)
  }else {
    ensemble_rank_overall <- ensemble_rank_scores$overall
    individual_rank_overall <- individual_rank_scores$overall
    overall_scores |>
      filter(time_period == 'test',
             !rt_ensemble_mod) |>
      group_by(k) |>
      summarize(min_score = min({{score_col_name}})/rt_ensemble_val,
                mean_score = mean({{score_col_name}})/rt_ensemble_val,
                max_score = max({{score_col_name}})/rt_ensemble_val) |>
      mutate(ensemble_type = 'Random') |> 
      bind_rows(ensemble_rank_overall |> 
                  filter(time_period == 'test') |> 
                  select({{score_col_name}}, k) |> 
                  mutate(mean_score = {{score_col_name}}/rt_ensemble_val,
                         min_score = NA,
                         max_score = NA,
                         ensemble_type = 'Ensemble rank') |> 
                  select(-{{score_col_name}}),
                individual_rank_overall |> 
                  filter(time_period == 'test') |> 
                  select({{score_col_name}}, k) |> 
                  mutate(mean_score = {{score_col_name}}/rt_ensemble_val,
                         min_score = NA,
                         max_score = NA,
                         ensemble_type = 'Individual rank') |> 
                  select(-{{score_col_name}})) |> 
      mutate(analysis = analysis_name)
  }
}

pull_rt_comparison_stats('case', avg_wis) |> 
  bind_rows(pull_rt_comparison_stats('death', avg_wis),
            pull_rt_comparison_stats('hosp2', avg_wis),
            pull_rt_comparison_stats('flu_hosp', avg_wis),
            pull_rt_comparison_stats('multiyear', score)) -> summary_rt_comparison

## Identify the improvement of RT ensemble for all metrics all ensembles
summary_rt_comparison |> 
  filter(ensemble_type == 'Random') |> 
  group_by(analysis) |> 
  filter(min_score == min(min_score)) |> 
  mutate(percent_improvement = min_score-1)


##Get table 1 information
analysis_order <- c('multiyear', 'flu_hosp', 'case', 'hosp2', 'death')
summary_rt_comparison |> 
  filter(k==4) |> 
  mutate(rel_score = ifelse(ensemble_type == 'Random',
                            paste0(round(mean_score, 2), ' (', 
                                   round(min_score, 2), '-', 
                                   round(max_score, 2), ')'),
                            round(mean_score, 2))) |> 
  select(ensemble_type, analysis, rel_score) |> 
  spread(ensemble_type, rel_score) |> 
  mutate(analysis = factor(analysis, levels = analysis_order)) |> 
  arrange(analysis) |>
  select(analysis, Random, `Individual rank`, `Ensemble rank`) |> 
  write_csv('processed-data/summary-4model-rel-rt.csv')



overall_scores |> 
  filter(time_period == 'test',
         !rt_ensemble_mod) |>
  group_by(k) |>
  summarize(score = mean(avg_wis)) |>
  filter(k == 5 | k == max(k)) |>
  mutate(k = ifelse(k == 5, 'start', 'finish')) |>
  spread(k, score) |>
  mutate(wane_percent = (finish - start)/start)

load(paste0('processed-data/death-score-summaries.rda'))
overall_scores |> 
  filter(time_period == 'test') |> 
  filter(avg_wis == min(avg_wis))
overall_scores |> 
  filter(time_period == 'test') |> 
  filter(rt_ensemble_mod)



