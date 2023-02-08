library(tidyverse)
library(cowplot)
library(ggrepel)
theme_set(theme_cowplot())

source('R/figure-helper-fxns.R')

refresh_data <- F

plot_metric_overall_performance(
  folder_path = 'processed-data/case-analysis',
  mod_info_path = 'raw-data/case-model-combination-lookup-table.csv',
  test_start_date = '2021-11-15',
  refresh_data = refresh_data) -> case_overall_fig
case_overall_fig


plot_metric_overall_performance(
  folder_path = 'processed-data/hosp2-analysis',
  mod_info_path = 'raw-data/hosp2-model-combination-lookup-table.csv',
  test_start_date = '2022-06-01',
  refresh_data = refresh_data) -> hosp_overall_fig
hosp_overall_fig

plot_metric_overall_performance(
  folder_path = 'processed-data/death-analysis',
  mod_info_path = 'raw-data/death-model-combination-lookup-table.csv',
  test_start_date = '2021-11-15',
  refresh_data = refresh_data) -> death_overall_fig
death_overall_fig

raw_figs <- plot_grid(case_overall_fig + theme(legend.position='none'), 
          hosp_overall_fig+ theme(legend.position='none'), 
          death_overall_fig+ theme(legend.position='none'), 
          nrow = 1, align = 'hv')

combined_fig <- plot_grid(raw_figs, get_legend(case_overall_fig), nrow = 1, rel_widths = c(1, .15))
save_plot('figs/combined-summary-fig.png', 
          combined_fig, 
          base_height = 4.5, 
          base_asp = 3,
          bg='white')




