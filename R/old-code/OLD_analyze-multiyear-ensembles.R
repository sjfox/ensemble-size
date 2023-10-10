library(tidyverse)
library(cowplot)
library(ggrepel)
library(here)
theme_set(theme_cowplot())

source('R/figure-helper-fxns.R')
source('R/plot-horizon-performance.R')

refresh <- F
my_files <- list.files(path = 'processed-data/multiyear-analysis/', full.names = T)
my_lookup <- read_csv('raw-data/multiyear-model-combination-lookup-table.csv') 

baseline_mod <- read_csv('raw-data/multiyear-model-combination-lookup-table.csv') %>% 
  filter(k == 1, model == 'ReichLab_kde') %>% 
  pull(combination_num)

target_bounds <- read_csv('raw-data/all-target-bounds.csv')
colnames(target_bounds) = tolower(colnames(target_bounds))


# Functions for analysis --------------------------------------------------
get_clean_forecast_score <- function(file_path, target_bounds){
  ## Reads in the ensemble scores for full time period
  ## Restricts to only forecast weeks that are contained within the flu season
  ## Similar to the previous multiyear analyses carried out
  ## e.g. https://www.pnas.org/doi/abs/10.1073/pnas.1812594116
  read_csv(file_path, col_types = 'ccnncc', show_col_types = FALSE) %>% 
    inner_join(target_bounds, by = c('season', 'location', 'target')) %>% 
    filter(ifelse(start_week>=38, 
                  (forecast_week>=start_week | forecast_week <= end_week),
                  (forecast_week>=start_week & forecast_week <= end_week)
    ))
}

get_overall_score <- function(file_path, target_bounds){
  get_clean_forecast_score(file_path, target_bounds) %>% 
    filter(season != '2010/2011') %>% 
    summarize(model = unique(model),
              score = exp(mean(score)), .groups = 'drop')
}
get_annual_score <- function(file_path,target_bounds){
  get_clean_forecast_score(file_path, target_bounds) %>% 
    group_by(season) %>% 
    summarize(model = unique(model),
              score = exp(mean(score)), .groups = 'drop')
}


# Run annual performance --------------------------------------
if(!file.exists('processed-data/modelyear-annual_performance.rda') |
   refresh){
  ## Annual performance for individual models, so we can create ensembles
  ## for individual rank ensembles
  ## Only needs individual model scores
  list.files('processed-data/multiyear-analysis/', full.names = T) %>% 
    map(get_annual_score, 
        target_bounds = target_bounds) %>% 
    bind_rows() -> my_annual_scores
  save(my_annual_scores, file = 'processed-data/multiyear-annual_performance.rda')
} else{
  load('processed-data/multiyear-annual_performance.rda')
}

## Create and score annual individual ensembles
my_ind_scores <- my_annual_scores %>% 
  mutate(combination_num = as.numeric(str_replace(model, pattern = 'ensemble-', replacement = ''))) %>% 
  left_join(my_lookup %>% distinct(combination_num, k)) %>% 
  filter(k == 1)
  
seasons <- my_ind_scores %>% pull(season) %>% unique()
source('R/multiyear-scoring-fxns.R')
if(!file.exists('processed-data/multiyear-ind_rank-ensemble.rda') | refresh){
  ind_rank_scores <- vector('list', 
                            length = nrow(my_ind_scores %>% 
                                            distinct(model)) * 
                              (length(seasons)-1))
  for(i in 2:length(seasons)){
    model_rank <- my_ind_scores %>% 
      filter(season == seasons[i-1]) %>% 
      separate(col = 'model', into = c('trash', 'combination_num')) %>%
      arrange(desc(score),.by_group = T) %>% 
      mutate(combination_num = as.numeric(combination_num)) %>% 
      left_join(my_lookup %>% filter(k==1))
    for(j in 1:nrow(model_rank)){
      model_rank %>% 
        slice(1:j) %>% 
        get_scored_seasons(raw_forecast_loc = 'raw-data',
                           save_loc = '',
                           model_name = 'ind_rank',
                           seasons = seasons[i],
                           save_scores = F) %>% 
        mutate(k = j) -> ind_rank_scores[[(i-2)*nrow(model_rank)+j]]
    }
  }
  ind_rank_scores %>% bind_rows() -> ind_rank_scores
  save(ind_rank_scores, file = 'processed-data/multiyear-ind_rank-ensemble.rda')
} else{
  load('processed-data/multiyear-ind_rank-ensemble.rda')
}

ind_rank_scores %>% 
  inner_join(target_bounds, by = c('season', 'location', 'target')) %>% 
  filter(ifelse(start_week>=38, 
                (forecast_week>=start_week | forecast_week <= end_week),
                (forecast_week>=start_week & forecast_week <= end_week)
  )) %>% 
  group_by(k) %>% 
  summarize(score = exp(mean(score))) -> ind_annual_score_summary



# Run the ensemble annual performance -------------------------------------
seasons <- my_annual_scores %>% pull(season) %>% unique()
ensemble_rank_scores <- vector('list', length = max(my_lookup$k))
for(curr_k in 1:max(my_lookup$k)){
  k_scores <- vector('list', length = length(seasons)-1)
  for(j in 2:length(seasons)){
    my_annual_scores %>%
      filter(season == seasons[j-1]) %>% 
      mutate(combination_num = as.numeric(str_replace(model, 'ensemble-', replacement = ''))) %>% 
      left_join(my_lookup %>% 
                  distinct(combination_num,k), by = 'combination_num') %>% 
      filter(k == curr_k) %>% 
      filter(score == max(score)) %>% 
      pull(model) -> top_model
    read_csv(paste0('processed-data/multiyear-analysis/',
                    top_model, '_scores.csv'),
             col_types = 'ccnncc', show_col_types = FALSE) %>% 
      filter(season == seasons[j]) %>% 
      mutate(k = curr_k) -> k_scores[[j-1]]
  }
  k_scores %>% bind_rows() ->ensemble_rank_scores[[curr_k]]
}
ensemble_rank_scores %>% 
  bind_rows() -> ensemble_rank_scores

ensemble_rank_scores %>% 
  inner_join(target_bounds, by = c('season', 'location', 'target')) %>% 
  filter(ifelse(start_week>=38, 
                (forecast_week>=start_week | forecast_week <= end_week),
                (forecast_week>=start_week & forecast_week <= end_week)
  )) %>% 
  group_by(k) %>% 
  summarize(score = exp(mean(score))) -> ens_annual_score_summary






## Get overall performance
if(!file.exists('processed-data/multiyear-overall_performance.rda') |
   refresh){
  my_files %>% 
    map(get_overall_score, 
        target_bounds = target_bounds) %>% 
    bind_rows() -> my_overall_scores
  save(my_overall_scores, file = 'processed-data/multiyear-overall_performance.rda')
} else{
  load('processed-data/multiyear-overall_performance.rda')
}





# Plot the overall multiyear figure ----------------------------------------

my_overall_scores %>% 
  mutate(combination_num = as.numeric(str_replace(model, 'ensemble-', replacement = ''))) -> my_overall_scores 

my_overall_scores %>% 
  left_join(my_lookup %>% distinct(combination_num,k), by = 'combination_num') %>% 
  group_by(k) %>% 
  summarize(min = min(score),
            max = max(score),
            mean = mean(score),
            median = median(score), .groups = 'drop') -> my_overall_summary


baseline_score <- my_overall_scores %>% 
  filter(combination_num == baseline_mod) %>% 
  pull(score)

## ensemble rank color: 
my_overall_summary %>% 
  ggplot(aes(k, 1/median)) + 
    geom_ribbon(aes(ymin = 1/min, ymax = 1/max), alpha = .1) +
    geom_line() +
    geom_line(data = ind_annual_score_summary, aes(x = k, y = 1/score), color = '#A16928', inherit.aes=F) +
    geom_line(data = ens_annual_score_summary, aes(x = k, y = 1/score), color = '#2887a1', inherit.aes=F) +
    geom_hline(yintercept = 1/baseline_score, lty = 2, color = 'grey30', size = 1) +
    geom_hline(yintercept = 1/tail(my_overall_summary$mean, 1), lty = 2, color = 'grey30', size = 1) +
    theme_cowplot() +
    coord_cartesian(ylim = c(12, 1/baseline_score*1.1)) +
    background_grid(major ='xy') +
    labs(x = 'Models included', y = 'Average score') +
    scale_x_continuous(breaks = 1:max(my_overall_summary$k, na.rm=T)) ->multiyr_overall_plot
multiyr_overall_plot
  
save_plot('figs/multiyr-overall-performance.png', 
          multiyr_overall_plot, 
          bg='white', 
          base_height = 5, base_asp = 1.2)





# Try different performance way -------------------------------------------

my_annual_scores |> 
  mutate(period = ifelse(season %in% c('2010/2011', '2011/2012', '2012/2013', '2013/2014'),
                         'train', 'test')) |> 
  group_by(model, period) |> 
  summarize(score = mean(score)) |> 
  ungroup() -> traintest_df

traintest_df |> 
  mutate(combination_num = as.numeric(str_replace(model, 'ensemble-', replacement = ''))) |> 
  filter(!is.na(combination_num)) |> 
  left_join(my_lookup  |>  distinct(combination_num,k), by = 'combination_num')  -> traintest_df

traintest_df |>  
  filter(period == 'test') |> 
  group_by(k) |> 
  summarize(min = min(score),
            max = max(score),
            mean = mean(score),
            median = median(score), .groups = 'drop') -> traintest_summary

traintest_df |> 
  filter(k==1) |> 
  filter(period == 'train') |> 
  arrange

traintest_summary |> 
  ggplot(aes(k, 1/median)) + 
  geom_ribbon(aes(ymin = 1/min, ymax = 1/max), alpha = .1) +
  geom_line() +
  # geom_line(data = ind_annual_score_summary, aes(x = k, y = 1/score), color = '#A16928', inherit.aes=F) +
  # geom_line(data = ens_annual_score_summary, aes(x = k, y = 1/score), color = '#2887a1', inherit.aes=F) +
  # geom_hline(yintercept = 1/baseline_score, lty = 2, color = 'grey30', size = 1) +
  # geom_hline(yintercept = 1/tail(my_overall_summary$mean, 1), lty = 2, color = 'grey30', size = 1) +
  theme_cowplot() +
  coord_cartesian(ylim = c(12, 1/baseline_score*1.1)) +
  background_grid(major ='xy') +
  labs(x = 'Models included', y = 'Average score') +
  scale_x_continuous(breaks = 1:max(my_overall_summary$k, na.rm=T))
  

