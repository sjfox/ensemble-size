library(tidyverse)
library(cowplot)
library(ggrepel)
theme_set(theme_cowplot())

source('R/figure-helper-fxns.R')
source('R/plot-ts-performance.R')


refresh_data <- F

plot_metric_ts_performance(
  folder_path = 'processed-data/case-analysis',
  mod_info_path = 'raw-data/case-model-combination-lookup-table.csv',
  test_start_date = '2021-11-15',
  refresh_data = refresh_data) -> case_ts_fig
case_ts_fig
save_plot('figs/case-ts-fig.png', 
          case_ts_fig, 
          base_height = 6, 
          base_asp = 1.5,
          bg='white')

plot_metric_ts_performance(
  folder_path = 'processed-data/hosp2-analysis',
  mod_info_path = 'raw-data/hosp2-model-combination-lookup-table.csv',
  test_start_date = '2022-06-01',
  refresh_data = refresh_data) -> hosp_ts_fig
hosp_ts_fig
save_plot('figs/hosp-ts-fig.png', 
          hosp_ts_fig, 
          base_height = 6, 
          base_asp = 1.5,
          bg='white')


plot_metric_ts_performance(
  folder_path = 'processed-data/death-analysis',
  mod_info_path = 'raw-data/death-model-combination-lookup-table.csv',
  test_start_date = '2021-11-15',
  refresh_data = refresh_data) -> death_ts_fig
death_ts_fig
save_plot('figs/death-ts-fig.png', 
          death_ts_fig, 
          base_height = 6, 
          base_asp = 1.5,
          bg='white')

plot_metric_ts_performance(
  folder_path = 'processed-data/flu_hosp-analysis',
  mod_info_path = 'raw-data/flu_hosp-model-combination-lookup-table.csv',
  test_start_date = '2022-06-21',## Actually train period end date for flu hosp
  refresh_data = F) -> fluhosp_ts_fig
fluhosp_ts_fig
save_plot('figs/fluhosp-ts-fig.png', 
          fluhosp_ts_fig, 
          base_height = 6, 
          base_asp = 1.5,
          bg='white')



