library(tidyverse)
library(cowplot)
library(ggrepel)
theme_set(theme_cowplot())

source('R/figure-helper-fxns.R')
source('R/plot-location-performance.R')


refresh_data <- F

plot_metric_location_performance(
  folder_path = 'processed-data/case-analysis',
  mod_info_path = 'raw-data/case-model-combination-lookup-table.csv',
  test_start_date = '2021-11-15',
  refresh_data = refresh_data) -> case_location_fig
case_location_fig
save_plot('figs/case-location-fig.png', 
          case_location_fig, 
          base_height = 4, 
          base_asp = 3,
          bg='white')

plot_metric_location_performance(
  folder_path = 'processed-data/hosp2-analysis',
  mod_info_path = 'raw-data/hosp2-model-combination-lookup-table.csv',
  test_start_date = '2022-06-01',
  refresh_data = refresh_data) -> hosp_location_fig
hosp_location_fig
save_plot('figs/hosp-location-fig.png', 
          hosp_location_fig, 
          base_height = 4, 
          base_asp = 3,
          bg='white')


plot_metric_location_performance(
  folder_path = 'processed-data/death-analysis',
  mod_info_path = 'raw-data/death-model-combination-lookup-table.csv',
  test_start_date = '2021-11-15',
  refresh_data = refresh_data) -> death_location_fig
death_location_fig
save_plot('figs/death-location-fig.png', 
          death_location_fig, 
          base_height = 4, 
          base_asp = 3,
          bg='white')

plot_metric_location_performance(
  folder_path = 'processed-data/flu_hosp-analysis',
  mod_info_path = 'raw-data/flu_hosp-model-combination-lookup-table.csv',
  test_start_date = '2022-06-20',## Actually train period end date for flu hosp
  refresh_data = refresh_data) -> fluhosp_location_fig
fluhosp_location_fig
save_plot('figs/fluhosp-location-fig.png', 
          fluhosp_location_fig, 
          base_height = 4, 
          base_asp = 3,
          bg='white')


# Combine all figures -----------------------------------------------------

raw_figs <- plot_grid(case_location_fig + theme(legend.position='none'), 
                      hosp_location_fig+ theme(legend.position='none'), 
                      death_location_fig+ theme(legend.position='none'), 
                      fluhosp_location_fig+ theme(legend.position='none'), 
                      nrow = 4, align = 'hv')
raw_figs
combined_location_fig <- plot_grid(raw_figs, 
                                   get_legend(case_location_fig + 
                                                theme(legend.position = 'bottom',
                                                      legend.justification = c(0.5,0.5))), 
                                   nrow = 2, rel_heights = c(1, .05))
save_plot('figs/combined-location-fig.png', 
          combined_location_fig, 
          base_height = 10, 
          base_asp = 0.9,
          bg='white')


