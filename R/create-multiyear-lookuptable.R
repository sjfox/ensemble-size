### Prepares table for multiyear analysis
set.seed(34738)
library(tidyverse)
source('R/data-pull-fxns.R')

folder_list <- list.files(path = 'raw-data/component-models', full.names = T)
folder_list <- folder_list[!grepl('*.csv', folder_list)]
get_model_name <- function(folder_path){
  model_name <- str_split(folder_path, pattern = '/')[[1]]
  model_name[length(model_name)]
}


model_names <- tibble(base_dir = folder_list,
                      model_name = folder_list %>% map(get_model_name) %>% unlist())
models <- model_names$model_name

tibble(k = 1:length(models),
       model_combinations = map(k, get_k_model_combinations, 
                                models = models)) %>% 
  select(-k) %>% 
  unnest(model_combinations) %>% 
  mutate(model = models[model]) %>% 
  nest(data = 'model') %>% 
  group_by(k) %>% 
  slice_sample(n = 1000, replace = F) %>% 
  ungroup() %>% 
  mutate(combination_num = seq_along(combo_num)) %>% 
  select(combination_num, k, data) -> model_combos

model_combos %>% 
  unnest(data) %>% 
  write_csv('raw-data/multiyear-model-combination-lookup-table.csv')

