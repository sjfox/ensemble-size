library(tidyverse)
library(cowplot)
library(ggrepel)
theme_set(theme_cowplot())

source('R/figure-helper-fxns.R')
source('R/plot-horizon-performance.R')


refresh_data <- F

plot_metric_horizon_performance(
  folder_path = 'processed-data/case-analysis',
  mod_info_path = 'raw-data/case-model-combination-lookup-table.csv',
  test_start_date = '2021-11-15',
  refresh_data = refresh_data) -> case_horizon_fig
case_horizon_fig
save_plot('figs/case-horizon-fig.png', 
          case_horizon_fig, 
          base_height = 4, 
          base_asp = 1.5,
          bg='white')

plot_metric_horizon_performance(
  folder_path = 'processed-data/hosp2-analysis',
  mod_info_path = 'raw-data/hosp2-model-combination-lookup-table.csv',
  test_start_date = '2022-06-01',
  refresh_data = refresh_data) -> hosp_horizon_fig
hosp_horizon_fig
save_plot('figs/hosp-horizon-fig.png', 
          hosp_horizon_fig, 
          base_height = 4, 
          base_asp = 1.5,
          bg='white')


plot_metric_horizon_performance(
  folder_path = 'processed-data/death-analysis',
  mod_info_path = 'raw-data/death-model-combination-lookup-table.csv',
  test_start_date = '2021-11-15',
  refresh_data = refresh_data) -> death_horizon_fig
death_horizon_fig
save_plot('figs/death-horizon-fig.png', 
          death_horizon_fig, 
          base_height = 4, 
          base_asp = 1.5,
          bg='white')

plot_metric_horizon_performance(
  folder_path = 'processed-data/flu_hosp-analysis',
  mod_info_path = 'raw-data/flu_hosp-model-combination-lookup-table.csv',
  test_start_date = '2022-06-20',## Actually train period end date for flu hosp
  refresh_data = refresh_data) -> fluhosp_horizon_fig
fluhosp_horizon_fig
save_plot('figs/fluhosp-horizon-fig.png', 
          fluhosp_horizon_fig, 
          base_height = 4, 
          base_asp = 1.5,
          bg='white')


# Combine all figures -----------------------------------------------------

raw_figs <- plot_grid(case_horizon_fig + theme(legend.position='none'), 
                      hosp_horizon_fig+ theme(legend.position='none'), 
                      death_horizon_fig+ theme(legend.position='none'), 
                      fluhosp_horizon_fig+ theme(legend.position='none'), 
                      nrow = 2, align = 'hv')
raw_figs
combined_horizon_fig <- plot_grid(raw_figs, get_legend(case_horizon_fig), nrow = 1, rel_widths = c(1, .22))
save_plot('figs/combined-horizon-fig.png', 
          combined_horizon_fig, 
          base_height = 6, 
          base_asp = 1.4,
          bg='white')


