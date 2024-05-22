library(tidyverse)
library(cowplot)
library(ggrepel)
theme_set(theme_cowplot())


calc_ensemble_strategy_difference <- function(summary_file_path,
                                              score_col_name,
                                              analysis_title){
  
  load(summary_file_path)
  # 
  # browser()
  
  if(grepl(pattern = 'multiyear', x = summary_file_path, fixed = T)){
    ensemble_rank_overall <- ensemble_rank_scores$overall_score
    individual_rank_overall <- individual_rank_scores$overall_score
    
    overall_scores |> 
      mutate(yval = 1 / {{score_col_name}}) -> overall_scores
    ensemble_rank_overall |> 
      mutate(yval = 1 / {{score_col_name}}) |> 
      mutate(k=as.numeric(str_replace(model, 'ens_rank-', replacement = ''))) -> ensemble_rank_overall
    individual_rank_overall |> 
      mutate(yval = 1 / {{score_col_name}}) |> 
      mutate(k=as.numeric(str_replace(model, 'ind_rank-', replacement = ''))) -> individual_rank_overall
  } else {
    ensemble_rank_overall <- ensemble_rank_scores$overall
    individual_rank_overall <- individual_rank_scores$overall
    
    overall_scores |> 
      mutate(yval = {{score_col_name}}) -> overall_scores
    ensemble_rank_overall |> 
      mutate(yval = {{score_col_name}}) -> ensemble_rank_overall
    individual_rank_overall |> 
      mutate(yval = {{score_col_name}}) -> individual_rank_overall
  }
  
  
  overall_scores |> 
    filter(baseline_mod, tolower(time_period) == 'test') |> 
    pull(yval) -> baseline_val
  
  
  ## Get ensemble rank values for plotting
  ensemble_rank_overall |> 
    filter(tolower(time_period) == 'test') |> 
    select(k, ens_rank = yval) -> ensemble_rank_vals
  
  ## Get individual rank values for plotting
  individual_rank_overall |> 
    filter(tolower(time_period) == 'test') |> 
    select(k, ind_rank = yval) -> ind_rank_vals
  

    
  
  ensemble_rank_overall |> 
    mutate(ensemble_score = yval/baseline_val) |> 
    select(time_period, k, ensemble_score) |> 
    left_join(individual_rank_overall |> 
                mutate(individual_score = yval/baseline_val) |> 
                select(time_period, k, individual_score),
              by = c('time_period', 'k')) |> 
    mutate(analysis_name = analysis_title) |> 
    left_join(  overall_scores |> 
                  filter(tolower(time_period) == 'test') |> 
                  filter(!(rt_ensemble_mod & k==1)) |> 
                  mutate(random_score = yval/baseline_val) |> 
                  select(k,random_score) |> 
                  nest(random_score = random_score), by = 'k')

}





calc_ensemble_strategy_difference(summary_file_path = 'processed-data/multiyear-score-summaries.rda', 
                         score_col_name = score,
                         analysis_title = 'ILI %') |> 
  bind_rows(calc_ensemble_strategy_difference(summary_file_path = 'processed-data/case-score-summaries.rda', 
                                              score_col_name = avg_wis,
                                              analysis_title = 'COVID-19 cases'),
            calc_ensemble_strategy_difference(summary_file_path = 'processed-data/hosp2-score-summaries.rda', 
                                              score_col_name = avg_wis,
                                              analysis_title = 'COVID-19 admissions'),
            calc_ensemble_strategy_difference(summary_file_path = 'processed-data/death-score-summaries.rda', 
                                              score_col_name = avg_wis,
                                              analysis_title = 'COVID-19 mortality'),
            calc_ensemble_strategy_difference(summary_file_path = 'processed-data/flu_hosp-score-summaries.rda', 
                                              score_col_name = avg_wis,
                                              analysis_title = 'Influenza admissions')) -> perf_df


perf_df |> 
  filter(tolower(time_period) == 'test') |> 
  mutate(ratio = ensemble_score/individual_score) |> 
  group_by(analysis_name) |> 
  mutate(k_frac = k/max(k)) |>
  ungroup() -> comp_df

comp_df |> 
  filter(k != 1, k_frac !=1) |> 
  count(analysis_name)
  

comp_df |> 
  group_by(analysis_name) |>
  unnest(random_score) |>
  # filter(k>2) |> 
  summarize(prob_ensemble_better_random = sum(ensemble_score/random_score<=1)/n(),
            prob_individual_better_random = sum(individual_score/random_score<=1)/n()) |> 
  left_join(comp_df |> 
              filter(k != 1, k_frac !=1) |> ## Removes the smallest and largest, because they will be equal
              group_by(analysis_name) |> 
              summarize(prob_ensemble_better_individual = sum(ratio<=1)/n(),
                        avg_improvement = mean((individual_score-ensemble_score)/individual_score),
                        avg_improvement_min = min((individual_score-ensemble_score)/individual_score),
                        avg_improvement_max = max((individual_score-ensemble_score)/individual_score),
                        )) |> 
  mutate(analysis_name = factor(analysis_name, levels = c('COVID-19 cases', 
                                                          'COVID-19 admissions',
                                                          'COVID-19 mortality',
                                                          'Influenza admissions',
                                                          'ILI %'))) |> 
  arrange(analysis_name) |> 
  mutate_at(vars(prob_ensemble_better_random, 
                 prob_individual_better_random, 
                 prob_ensemble_better_individual), 
            scales::percent, 
            accuracy = .1) |> 
  mutate(avg_improvement = paste0(scales::percent(avg_improvement, accuracy=.1), 
                                  ' (', scales::comma(avg_improvement_min*100, accuracy=.1), '-', scales::percent(avg_improvement_max, accuracy=.1),')')) |> 
  select(analysis_name, prob_individual_better_random, prob_ensemble_better_random, prob_ensemble_better_individual, avg_improvement) |> 
  write_csv('processed-data/summary-scores-individual-vs-ensemble.csv')
            

mean(c(87.2, 33.1, 64.1, 61.8,71.8))

mean(c(89.0,
70.9,
82.6,
99.7,
97.3))

mean(c(66.70,
100.00,
87.50,
95.20,
100.00))

mean(c(1.5,
11.9,
1.3,
8.1,
8.1))




rsv_truth_1 <- rsv_truth %>%
  arrange(location_name, date) %>%  # Sort by location_name and date
  group_by(location_name) %>%  # Group by location_name
  mutate(first_diff = count - lag(count)) %>%  # Calculate the first difference
  summarize(frac_of_peak = min(abs(first_diff), na.rm = TRUE) / max(abs(count), na.rm = TRUE)) |> 
  


comp_df |> 
  filter(k == 4) |> 
  unnest(random_score) |> 
  group_by(analysis_name) |> 
  summarize(ensemble_score = unique(ensemble_score),
            individual_score = unique(individual_score),
            random_score_mean = mean(random_score),
            random_score_lb = quantile(random_score, probs = 0.025),
            random_score_ub = quantile(random_score, probs = 0.975)) |> 
  mutate(ensemble_improvement = (individual_score-ensemble_score)/individual_score) |> 
  select(analysis_name, individual_score, ensemble_score, ensemble_improvement, random_score_mean:random_score_ub) |> 
  left_join(comp_df |> 
              filter(k != 1, k_frac !=1) |> 
              group_by(analysis_name) |> 
              summarize(avg_improvement = mean((individual_score-ensemble_score)/individual_score),
                        avg_improvement_min = quantile((individual_score-ensemble_score)/individual_score, probs = 0.025),
                        avg_improvement_max = quantile((individual_score-ensemble_score)/individual_score, probs = 0.975),
                        prob_match_better = sum(ratio<=1)/n())
  ) |> 
  arrange(analysis_name) |> 
  mutate_at(vars(individual_score, ensemble_score), scales::comma, accuracy = .01) |> 
  mutate_at(vars(ensemble_improvement, prob_match_better), scales::percent, accuracy = .1) |> 
  mutate(random_score = paste0(scales::comma(random_score_mean, accuracy=.01), 
                               ' (', scales::comma(random_score_lb, accuracy=.01), '-', scales::comma(random_score_ub, accuracy=.01),')'),
         avg_improvement = paste0(scales::percent(avg_improvement, accuracy=.1), 
                                  ' (', scales::comma(avg_improvement_min*100, accuracy=.1), '-', scales::percent(avg_improvement_max, accuracy=.1),')')) |> 
  select(-avg_improvement_min, -avg_improvement_max,-random_score_mean,-random_score_lb, -random_score_ub)
  write_csv('processed-data/summary-scores-individual-vs-ensemble.csv')



# comp_df |>   
#   filter(k_frac !=0, k_frac != 1) |> 
#   group_by(analysis_name) |> 
#   summarize(avg = mean(ratio),
#             prop = sum(ratio<=1)/n())
# 
# comp_df |> 
#   filter(k_frac !=0, k_frac != 1) |> 
#   # filter(analysis_name == 'ILI %') |> 
# # filter(k!=min(k), k != max(k)) |> 
#   ggplot(aes(k_frac, ratio)) +
#   geom_point() +
#   geom_smooth(se =F)
# 
# comp_df |> 
#   filter(k == 5) |> 
#   select(ensemble_score:analysis_name) |> 
#   gather(key,value, -analysis_name) |> 
#   group_by(analysis_name) |> 
#   mutate(value = value/mean(value)) |> 
#   ggplot(aes(analysis_name, value, fill = key, color = key)) +
#     geom_col(position = position_dodge())