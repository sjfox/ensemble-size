## Pull statistics for manuscript
library(tidyverse)

tibble(files = list.files('raw-data/', pattern = 'model-combination-lookup-table.csv', full.names = T),
       num_combinations = list.files('raw-data/', pattern = 'model-combination-lookup-table.csv', full.names = T) |> 
         map(read_csv) |> 
         map(.f = function(x) {x |> pull(combination_num) |> max()}) |> 
         unlist())

sum(choose(7,1:7))


## Get the case decline in performance when including more models
load(paste0('processed-data/flu_hosp-score-summaries.rda'))
overall_scores |> 
  filter(time_period == 'test',
         !rt_ensemble_mod) |>
  group_by(k) |>
  summarize(score = mean(avg_wis)) |>
  filter(k == 4 | k == 7) |>
  mutate(k = ifelse(k == 4, 'start', 'finish')) |>
  spread(k, score) |>
  mutate(wane_percent = (finish - start)/start)


overall_scores |> 
  filter(time_period == 'test',
         !rt_ensemble_mod) |> 
  filter(k == 4 | k == 7) |>
  group_by(k) |> 
  summarize(iqr = IQR(avg_wis)) |> 
  mutate(k = ifelse(k == 4, 'start', 'finish')) |>
  spread(k, iqr) |>
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


# Investigate consistency of performance ----------------------------------
# overall_scores |> 
#   filter(baseline_mod) |> 
#   select(time_period, baseline=avg_wis) -> baseline_scores

## Assign all models a random rank
## permutation test 

## No longer doing permutation test
# overall_scores |> 
#   filter(!rt_ensemble_mod) |> 
#   group_by(time_period,k) |> 
#   # mutate(avg_wis = 1/score) |> 
#   arrange(avg_wis) |> 
#   mutate(ranking = seq_along(avg_wis)) |> 
#   ungroup() |> 
#   select(k, time_period, model, ranking) |> 
#   spread(time_period, ranking) |> 
#   group_by(k)-> base_ranks 
# 
# 
# map(1:1000, ~{base_ranks |> 
#   mutate(test_perm = sample(test, length(test), replace=F)) |> 
#   mutate(diff = abs(test-train),
#          diff_perm = abs(test_perm-train)) |> 
#   summarize(avg_diff = mean(diff),
#             avg_perm_diff = mean(diff_perm))}) |> 
#   bind_rows(.id = 'sample') -> temp
# 
# 
# temp |> 
#   group_by(k) |> 
#   summarize(proportion_closer_rank = sum(avg_diff < avg_perm_diff)/n()) 


get_rank_prop_comparison <- function(summary_file_path, prop = .1){
  # browser()
  load(summary_file_path)
  if(grepl(pattern = 'multiyear',summary_file_path)){
    overall_scores |> 
      mutate(time_period = tolower(time_period),
             avg_wis = 1/score) -> overall_scores
  } 
  return(overall_scores)
  # 
  # overall_scores |> 
  #   filter(!rt_ensemble_mod) |> 
  #   group_by(time_period, k) |> 
  #   # mutate(avg_wis = 1/score) |>
  #   arrange(avg_wis) |> 
  #   mutate(ranking = seq_along(avg_wis)/n()) |> 
  #   ungroup() |> 
  #   select(k, time_period, model, ranking) |> 
  #   spread(time_period, ranking) |> 
  #   filter(train < prop) |> 
  #   filter(!is.na(test)) |> 
  #   group_by(k) |> 
  #   summarize(prop_below_threshold = sum(test < prop)/n()) 
  

  # overall_scores |>
  #   filter(!rt_ensemble_mod) |>
  #   group_by(time_period) |>
  #   # mutate(avg_wis = 1/score) |>
  #   arrange(avg_wis) |>
  #   mutate(ranking = seq_along(avg_wis)/n()) |>
  #   ungroup() |>
  #   select(k, time_period, model, ranking) |>
  #   spread(time_period, ranking) |>
  #   filter(train < prop) |>
  #   filter(!is.na(test)) |>
  #   summarize(prop_below_threshold = sum(test < prop)/n()) |>
  #   pull(prop_below_threshold)
  
}

tibble(file_path = c("processed-data/case-score-summaries.rda", 
                     "processed-data/death-score-summaries.rda", 
                     "processed-data/flu_hosp-score-summaries.rda",
                     "processed-data/hosp2-score-summaries.rda",
                     "processed-data/multiyear-score-summaries.rda"),
       analysis_name = c('COVID-19 cases',
                         'COVID-19 mortality',
                         'Influenza admissions',
                         'COVID-19 admissions',
                         'ILI %')) |> 
  mutate(threshold = .1) |> 
  mutate(prop_topten_remain_topten = map2(file_path, threshold,
                                          get_rank_prop_comparison)) |> 
  unnest(prop_topten_remain_topten) -> df
  # mutate(likelihood = prop_topten_remain_topten/threshold) -> prob_ranks_df

df |> 
  filter(!rt_ensemble_mod) |>
  group_by(analysis_name, time_period, k) |>
  arrange(avg_wis) |>
  mutate(ranking = seq_along(avg_wis)/n()) |> 
  ungroup() |>
  select(analysis_name, k, time_period, model, ranking) |> 
  spread(time_period, ranking) |> 
  group_by(analysis_name) |> 
  filter(k != max(k)) |> 
  group_by(analysis_name, k) |> 
  filter(train < .25) |>
  filter(!is.na(test)) |>
  summarize(prop_below_threshold = sum(test < .25)/n()) |> 
  ggplot(aes(k, prop_below_threshold, color = analysis_name)) + geom_line()




library(cowplot)
prob_ranks_df |> 
  ggplot(aes(analysis_name, prop_topten_remain_topten)) +
  geom_col() +
  theme_cowplot() +
  labs(x = NULL, y = 'Probability remain in top 10%') +
  geom_hline(yintercept = .1, lty = 2, color = 'red') +
  # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  background_grid(major = 'y', minor = 'y') +
  scale_y_continuous(expand = c(0,0))



# Pull statistics about relative to R of forecasts --------------------
pull_rt_comparison_stats <- function(analysis_name, score_col_name){
  # browser()
  load(paste0('processed-data/', analysis_name, '-score-summaries.rda'))
  
  ## Get rt ensemble value 
  overall_scores |> 
    filter(rt_ensemble_mod, tolower(time_period) == 'test') |> 
    pull({{score_col_name}}) -> rt_ensemble_val
  
  overall_scores |> 
    filter(k==max(k), tolower(time_period) == 'test') |> 
    pull({{score_col_name}}) -> full_included_score
  
  browser()
  if(analysis_name == 'multiyear'){
    ensemble_rank_overall <- ensemble_rank_scores$overall_score
    individual_rank_overall <- individual_rank_scores$overall_score
    
    rt_ensemble_val <- 1/rt_ensemble_val
    full_included_score <- 1/full_included_score
    
    overall_scores |>
      filter(tolower(time_period) == 'test') |>
      group_by(k) |>
      summarize(min_score = min(1/{{score_col_name}})/rt_ensemble_val,
                mean_score = mean(1/{{score_col_name}})/rt_ensemble_val,
                max_score = max(1/{{score_col_name}})/rt_ensemble_val,
                prop_below_rt = sum(1/{{score_col_name}}/rt_ensemble_val<=1)/n(),
                prop_below_full = sum(1/{{score_col_name}}/full_included_score<=1)/n()) |>
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
                max_score = max({{score_col_name}})/rt_ensemble_val,
                prop_below_rt = sum({{score_col_name}}/rt_ensemble_val <= 1)/n(),
                prop_below_full = sum({{score_col_name}}/full_included_score <= 1)/n()) |>
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
library(ggridges)
overall_scores |>
  filter(time_period == 'Test') |> 
  ggplot(aes(x = 1/score, y = as.factor(k))) + 
  geom_density_ridges() +
  coord_cartesian(xlim = c(13,30))


pull_rt_comparison_stats('case', avg_wis) |> 
  bind_rows(pull_rt_comparison_stats('death', avg_wis),
            pull_rt_comparison_stats('hosp2', avg_wis),
            pull_rt_comparison_stats('flu_hosp', avg_wis),
            pull_rt_comparison_stats('multiyear', score)) -> summary_rt_comparison

summary_rt_comparison |> 
  filter(ensemble_type == 'Random') |> 
  group_by(analysis) |> 
  filter(k!=max(k)) |> 
  mutate(prop_k = k/max(k)) |> 
  ggplot(aes(prop_k, prop_below_full, color = analysis)) +
  geom_line() +
  labs(x = 'Proportion of full ensemble', 
       y = 'P(Random ensemble beats max K ensemble)')


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


ensemble_rank_overall |> 
  pull(model) |> unique() ->temp


scores_by_location |>
  filter(time_period == 'test',
         model %in% temp) |> 
  filter(k == 4) |> 
  left_join(scores_by_location |> 
              filter(rt_ensemble_mod, time_period == 'test') |> 
              select(location, rt_avg_wis = avg_wis), by = 'location') |> 
  mutate(rel_score = avg_wis/rt_avg_wis) |> 
  summarize(mean(rel_score),
            quantile(rel_score, probs = 0.25),
            quantile(rel_score, probs = 0.75))
rt_ensemble_val

((overall_scores |>
  filter(time_period == 'test',
         model %in% temp) |> pull(avg_wis))/rt_ensemble_val) |> summary()
