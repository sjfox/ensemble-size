library(tidyverse)
library(cowplot)
library(ggrepel)
theme_set(theme_cowplot())


plot_pi_performance <- function(summary_file_path,
                                score_col_name,
                                plot_title){
  
  load(summary_file_path)
  # 
  # browser()
  ensemble_rank_overall <- ensemble_rank_scores$overall
  individual_rank_overall <- individual_rank_scores$overall
  overall_scores |> 
    mutate(yval = {{score_col_name}}) -> overall_scores
  ensemble_rank_overall |> 
    mutate(yval = {{score_col_name}}) -> ensemble_rank_overall
  individual_rank_overall |> 
    mutate(yval = {{score_col_name}}) -> individual_rank_overall
  
  ## Get rt ensemble value for plotting
  overall_scores |> 
    filter(rt_ensemble_mod, tolower(time_period) == 'test') |> 
    pull(yval) -> rt_ensemble_val
  
  ## Get baseline value for plotting
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
  
  ## Get random ensemble values for plotting
  overall_scores |> 
    filter(tolower(time_period) == 'test') |> 
    group_by(k) |> 
    summarize(min_val = min(yval),
              mean_val = mean(yval),
              max_val = max(yval)) -> random_ensemble_vals
  # browser()
  random_ensemble_vals |> 
    select(k, Random = mean_val) |> 
    left_join(ensemble_rank_vals, by = 'k') |> 
    left_join(ind_rank_vals, by = 'k') |> 
    rename(`Ensemble rank` = ens_rank,
           `Individual rank` = ind_rank) |> 
    gather(key, value, -k) |> 
    ggplot(aes(k, value, color = key, fill = key)) +
    geom_ribbon(data = random_ensemble_vals |> 
                  mutate(key = 'Random'),
                aes(x = k, ymin = min_val, ymax = max_val, fill = key), 
                alpha = .1, color = NA, inherit.aes=F)   +
    geom_line(size = .7) +
    scale_color_manual(values = c('#A16928', '#2887a1', 'black')) +
    scale_fill_manual(values = c('white', 'white', 'black')) +
    geom_hline(data = tibble(line_name = c('Real-time ensemble', 'Baseline'),
                             yval = c(rt_ensemble_val,
                                      baseline_val)),
               aes(yintercept = yval, lty = line_name), color = 'grey30', size = .65) +
    scale_linetype_manual(values = c(3,2)) +
    coord_cartesian(ylim = c(min(random_ensemble_vals$min_val)*.975, 1)) +
    # background_grid(major ='xy') +
    labs(x = 'Number of included models', 
         y = '90% PI coverage', 
         title = plot_title, 
         color = NULL, 
         lty = NULL,
         fill = NULL) +
    theme(legend.spacing.y = unit(0.0, "cm")) +
    guides(color = guide_legend(override.aes = list(linewidth = 1), order = 1),
           fill = guide_legend(order = 1),
           linetype = guide_legend(override.aes = list(linewidth = 1), order = 2)) +
    scale_x_continuous(breaks = 1:max(random_ensemble_vals$k)) +
    scale_y_continuous(labels = scales::percent) +
    NULL
}


## Covid-19 case overall plot
plot_pi_performance(summary_file_path = 'processed-data/case-score-summaries.rda', 
                         score_col_name = q90,
                         plot_title = 'COVID-19 cases') -> covidcase_pi_overall
covidcase_pi_overall
save_plot( filename = 'figs/covidcase_pi_overall.png', 
           covidcase_pi_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')

## Covid-19 hospital admissions overall plot
plot_pi_performance(summary_file_path = 'processed-data/hosp2-score-summaries.rda', 
                         score_col_name = q90,
                         plot_title = 'COVID-19 admissions') -> covidadmits_pi_overall
covidadmits_pi_overall
save_plot( filename = 'figs/covidadmits_pi_overall.png', 
           covidadmits_pi_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')

## Covid-19 mortality overall plot
plot_pi_performance(summary_file_path = 'processed-data/death-score-summaries.rda', 
                         score_col_name = q90,
                         plot_title = 'COVID-19 mortality') -> coviddeaths_pi_overall
coviddeaths_pi_overall
save_plot( filename = 'figs/coviddeaths_pi_overall.png', 
           coviddeaths_pi_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')

## Flu admissions overall plot
plot_pi_performance(summary_file_path = 'processed-data/flu_hosp-score-summaries.rda', 
                         score_col_name = q90,
                         plot_title = 'Influenza admissions') -> fluadmits_pi_overall
fluadmits_pi_overall
save_plot( filename = 'figs/fluadmits_pi_overall.png', 
           fluadmits_pi_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')


## Put together into a single figure.

pi_panel_plot <- plot_grid(
  covidcase_pi_overall + theme(legend.position='none'), 
  covidadmits_pi_overall + theme(legend.position='none'), 
  coviddeaths_pi_overall + theme(legend.position='none'), 
  fluadmits_pi_overall + theme(legend.position='none'),
  nrow = 2, align = 'hv') |> 
  plot_grid(get_legend(covidcase_pi_overall + 
                         guides(color = guide_legend(label.theme = element_text(size = 16),
                                                     keywidth = 2, 
                                                     keyheight = 2,
                                                     override.aes = list(linewidth = 2), order = 1),
                                fill = guide_legend(order = 1),
                                linetype = guide_legend(label.theme = element_text(size = 16),
                                                        keywidth = 2, 
                                                        keyheight = 2,
                                                        override.aes = list(linewidth = 2)))),
            nrow = 1, rel_widths = c(2.8,1)
  )
pi_panel_plot

save_plot('figs/overall-pi-summary-fig.png', 
          pi_panel_plot, 
          base_height = 7, 
          base_asp = 1.4,
          bg='white')



