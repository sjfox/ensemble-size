library(tidyverse)
library(cowplot)
library(ggrepel)
theme_set(theme_cowplot())

source('R/figure-helper-fxns.R')
source('R/plot-overall-performance.R')
refresh_data <- F

plot_metric_overall_performance(
  folder_path = 'processed-data/case-analysis',
  mod_info_path = 'raw-data/case-model-combination-lookup-table.csv',
  test_start_date = '2021-11-15',
  refresh_data = refresh_data) -> case_overall_fig
case_overall_fig
save_plot('figs/case-overall-fig.png', 
          case_overall_fig, 
          base_height = 4, 
          base_asp = 1.5,
          bg='white')

plot_metric_overall_performance(
  folder_path = 'processed-data/hosp2-analysis',
  mod_info_path = 'raw-data/hosp2-model-combination-lookup-table.csv',
  test_start_date = '2022-06-01',
  refresh_data = refresh_data) -> hosp_overall_fig
hosp_overall_fig
save_plot('figs/hosp-overall-fig.png', 
          hosp_overall_fig, 
          base_height = 4, 
          base_asp = 1.5,
          bg='white')


plot_metric_overall_performance(
  folder_path = 'processed-data/death-analysis',
  mod_info_path = 'raw-data/death-model-combination-lookup-table.csv',
  test_start_date = '2021-11-15',
  refresh_data = refresh_data) -> death_overall_fig
death_overall_fig
save_plot('figs/death-overall-fig.png', 
          death_overall_fig, 
          base_height = 4, 
          base_asp = 1.5,
          bg='white')

plot_metric_overall_performance(
  folder_path = 'processed-data/flu_hosp-analysis',
  mod_info_path = 'raw-data/flu_hosp-model-combination-lookup-table.csv',
  test_start_date = '2022-10-01',
  refresh_data = refresh_data) -> fluhosp_overall_fig
fluhosp_overall_fig
save_plot('figs/fluhosp-overall-fig.png', 
          fluhosp_overall_fig, 
          base_height = 4, 
          base_asp = 1.5,
          bg='white')

raw_figs <- plot_grid(case_overall_fig + theme(legend.position='none'), 
          hosp_overall_fig+ theme(legend.position='none'), 
          death_overall_fig+ theme(legend.position='none'), 
          fluhosp_overall_fig+ theme(legend.position='none'), 
          nrow = 2, align = 'hv')
raw_figs
combined_fig <- plot_grid(raw_figs, get_legend(case_overall_fig), nrow = 1, rel_widths = c(1, .22))
save_plot('figs/combined-summary-fig.png', 
          combined_fig, 
          base_height = 6, 
          base_asp = 1.4,
          bg='white')




