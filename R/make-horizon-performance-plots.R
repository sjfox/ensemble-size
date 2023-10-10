library(tidyverse)
library(cowplot)
library(ggrepel)
theme_set(theme_cowplot())


plot_horizon_performance <- function(summary_file_path,
                                score_col_name,
                                truth_yname,
                                score_yname){
  
  load(summary_file_path)
  # 
  # browser()
  
  model_colors <- tibble(model = c('Baseline', 'Ensemble rank', 'Individual rank', 'Random', 'Real-time ensemble'),
                         color = c('darkgrey', '#A16928', '#2887a1', 'black', '#764E9F'))
  
  if(grepl(pattern = 'multiyear', x = summary_file_path, fixed = T)){
    ensemble_rank_scores <- ensemble_rank_scores$score_by_fcast_horizon
    individual_rank_scores <- individual_rank_scores$score_by_fcast_horizon
    
    scores_by_fcast_horizon |> 
      mutate(yval = 1 / {{score_col_name}}) -> scores_by_fcast_horizon
    ensemble_rank_scores |> 
      mutate(yval = 1 / {{score_col_name}}) |> 
      mutate(k=as.numeric(str_replace(model, 'ens_rank-', replacement = ''))) -> ensemble_rank_scores
    individual_rank_scores |> 
      mutate(yval = 1 / {{score_col_name}}) |> 
      mutate(k=as.numeric(str_replace(model, 'ind_rank-', replacement = ''))) -> individual_rank_scores
    
    ## Get rt ensemble value for plotting
    scores_by_fcast_horizon |> 
      filter(rt_ensemble_mod)  -> rt_ensemble_vals
    
    ## Get baseline value for plotting
    scores_by_fcast_horizon |> 
      filter(baseline_mod) -> baseline_vals
    
    ## Get ensemble rank values for plotting
    ensemble_rank_scores |> 
      filter(k==4)  -> ensemble_rank_vals
    
    ## Get individual rank values for plotting
    individual_rank_scores |> 
      filter(k==4)  -> individual_rank_vals
    
    ## Get random ensemble values for plotting
    scores_by_fcast_horizon |> 
      filter(!rt_ensemble_mod, k == 4) |> 
      group_by(time_period, target) |> 
      summarize(min_val = min(yval),
                mean_val = mean(yval),
                max_val = max(yval)) -> random_ensemble_vals
    
    # browser()
    random_ensemble_vals |> 
      select(time_period, target, yval = mean_val) |> 
      mutate(model = 'Random') |> 
      bind_rows(rt_ensemble_vals |> 
                  select(time_period, target, yval = yval) |> 
                  mutate(model = 'Real-time ensemble'),
                baseline_vals |> 
                  select(time_period, target, yval = yval) |> 
                  mutate(model = 'Baseline'),
                ensemble_rank_vals |> 
                  select(time_period, target, yval = yval) |> 
                  mutate(model = 'Ensemble rank'),
                individual_rank_vals |> 
                  select(time_period, target, yval = yval) |> 
                  mutate(model = 'Individual rank')) |> 
      ungroup() |> 
      filter(time_period == 'Test') |> 
      ggplot(aes(model, yval, fill = model)) +
      geom_col() +
      facet_wrap(~target, scales = 'free_y', nrow = 2) +
      labs(x = NULL, y = score_yname, fill = NULL) +
      background_grid(major = 'y') +
      scale_fill_manual(values = model_colors$color)  +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')
  } else {
    ensemble_rank_scores <- ensemble_rank_scores$fcast_horizon
    individual_rank_scores <- individual_rank_scores$fcast_horizon
    
    scores_by_fcast_horizon |> 
      mutate(yval = {{score_col_name}}) -> scores_by_fcast_horizon
    ensemble_rank_scores |> 
      mutate(yval = {{score_col_name}})  -> ensemble_rank_scores
    individual_rank_scores |> 
      mutate(yval = {{score_col_name}})  -> individual_rank_scores
    
    ## Get rt ensemble value for plotting
    scores_by_fcast_horizon |> 
      filter(rt_ensemble_mod)  -> rt_ensemble_vals
    
    ## Get baseline value for plotting
    scores_by_fcast_horizon |> 
      filter(baseline_mod) -> baseline_vals
    
    ## Get ensemble rank values for plotting
    ensemble_rank_scores |> 
      filter(k==4)  -> ensemble_rank_vals
    
    ## Get individual rank values for plotting
    individual_rank_scores |> 
      filter(k==4)  -> individual_rank_vals
    
    ## Get random ensemble values for plotting
    scores_by_fcast_horizon |> 
      filter(!rt_ensemble_mod, k == 4) |> 
      group_by(time_period, horizon) |> 
      summarize(min_val = min(yval),
                mean_val = mean(yval),
                max_val = max(yval)) -> random_ensemble_vals
    
    
    random_ensemble_vals |> 
      select(time_period, horizon, yval = mean_val) |> 
      mutate(model = 'Random') |> 
      bind_rows(rt_ensemble_vals |> 
                  select(time_period, horizon, yval = yval) |> 
                  mutate(model = 'Real-time ensemble'),
                baseline_vals |> 
                  select(time_period, horizon, yval = yval) |> 
                  mutate(model = 'Baseline'),
                ensemble_rank_vals |> 
                  select(time_period, horizon, yval = yval) |> 
                  mutate(model = 'Ensemble rank'),
                individual_rank_vals |> 
                  select(time_period, horizon, yval = yval) |> 
                  mutate(model = 'Individual rank')) |> 
      ungroup() |> 
      filter(time_period == 'test') |> 
      ggplot(aes(horizon, yval, color = model)) +
      geom_line() +
      labs(x = 'Forecast horizon', y = score_yname, color = NULL,
           title = truth_yname) +
      background_grid(major = 'xy', minor = 'xy') +
      scale_color_manual(values = model_colors$color) +
      guides(color = guide_legend(override.aes = list(linewidth = 5))) 
  }
}


## Multiyear overall plot
plot_horizon_performance(summary_file_path = 'processed-data/multiyear-score-summaries.rda', 
                    score_col_name = score,
                    truth_yname = 'ILI %',
                    score_yname = 'Forecast score') -> my_horizon
my_horizon
save_plot( filename = 'figs/my_horizon.png', 
           my_horizon,
           base_height = 7,
           base_asp = 1.6,
           bg = 'white')

## Covid-19 case overall plot
plot_horizon_performance(summary_file_path = 'processed-data/case-score-summaries.rda', 
                          score_col_name = avg_wis,
                          truth_yname = 'COVID-19 cases',
                          score_yname = 'WIS') -> case_horizon
case_horizon

## Covid-19 hospital admissions overall plot
plot_horizon_performance(summary_file_path = 'processed-data/hosp2-score-summaries.rda', 
                    score_col_name = avg_wis,
                    truth_yname = 'COVID-19 admissions',
                    score_yname = 'WIS') -> covidadmit_horizon
covidadmit_horizon


## Covid-19 mortality overall plot
plot_horizon_performance(summary_file_path = 'processed-data/death-score-summaries.rda', 
                    score_col_name = avg_wis,
                    truth_yname = 'COVID-19 mortality',
                    score_yname = 'WIS') -> deaths_horizon
deaths_horizon


## Flu admissions overall plot
plot_horizon_performance(summary_file_path = 'processed-data/flu_hosp-score-summaries.rda', 
                    score_col_name = avg_wis,
                    truth_yname = 'Influenza admissions',
                    score_yname = 'WIS') -> fluadmits_horizon
fluadmits_horizon



## Put together into a single figure.

panel_plot <- plot_grid(
  case_horizon + theme(legend.position='none'), 
  covidadmit_horizon + theme(legend.position='none'), 
  deaths_horizon + theme(legend.position='none'), 
  fluadmits_horizon + theme(legend.position='none'),
  nrow = 2, align = 'hv') |> plot_grid(get_legend(case_horizon), nrow = 1, rel_widths = c(1, .2))
panel_plot

save_plot('figs/horizon-summary.png', 
          panel_plot, 
          base_height = 6, 
          base_asp = 1.6,
          bg='white')


