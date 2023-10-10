## 2023 flu analysis
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())


load('processed-data/2023flu-score-summaries.rda')


overall_scores |> filter(is.na(avg_wis))

overall_scores |> 
  filter(baseline_mod) |> pull(avg_wis) -> baseline_performance

overall_scores |> 
  filter(rt_ensemble_mod)|> pull(avg_wis) -> rt_performance

overall_scores |> 
  filter(!is.na(avg_wis)) |> 
  filter(!rt_ensemble_mod) |> 
  group_by(k) |> 
  summarize(mean = mean(avg_wis),
            lo = min(avg_wis),
            hi = max ( avg_wis)) |> 
  ggplot(aes(k, mean)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), color = NA, alpha = .2) +
    geom_line() +
    geom_hline(yintercept = baseline_performance, lty = 3) +
    scale_x_continuous(breaks = 1:max(overall_scores$k)) +
    labs(x = 'Number of models included', y = 'Average WIS') +
    geom_hline(yintercept = rt_performance, lty = 2) -> flu2023_overall_performance
flu2023_overall_performance
save_plot('figs/flu2023_overall_performance.png', flu2023_overall_performance,
          base_height = 5, base_asp = 1.3, bg = 'white')



## Model combination lookup
model_lookup <- read_csv('raw-data/2023flu-model-combination-lookup-table.csv')

overall_scores |> 
  filter(!rt_ensemble_mod) |> 
  group_by(k) |> 
  filter(avg_wis == min(avg_wis,na.rm=T)) |> 
  arrange(k) |> 
  select(-model) |> 
  left_join(model_lookup |> select(-k), by = 'combination_num') -> temp

temp |> write_csv('processed-data/2023flu-best-ensemble-rank-withgt.csv')

model_lookup |> filter(model == 'GT-FluFNP') |> 
  pull(combination_num) |> unique() -> excluded_combos

overall_scores |> 
  filter(!rt_ensemble_mod) |> 
  filter(! (combination_num %in% excluded_combos)) |> 
  group_by(k) |> 
  filter(avg_wis == min(avg_wis,na.rm=T)) |> 
  arrange(k) |> 
  select(-model) |> 
  left_join(model_lookup |> select(-k), by = 'combination_num') -> temp

temp |> write_csv('processed-data/2023flu-best-ensemble-rank-nogt.csv')

