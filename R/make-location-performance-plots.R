library(tidyverse)
library(cowplot)
library(ggrepel)
theme_set(theme_cowplot())


plot_location_performance <- function(summary_file_path,
                                     score_col_name,
                                     truth_yname,
                                     score_yname){
  
  load(summary_file_path)
  # 
  # browser()
  
  model_colors <- tibble(model = c('Baseline', 'Ensemble rank', 'Individual rank', 'Random', 'Published ensemble'),
                         color = c('darkgrey', '#A16928', '#2887a1', 'black', '#764E9F'))
  
  if(grepl(pattern = 'multiyear', x = summary_file_path, fixed = T)){
    ensemble_rank_scores <- ensemble_rank_scores$score_by_location
    individual_rank_scores <- individual_rank_scores$score_by_location
    
    scores_by_location |> 
      mutate(yval = 1 / {{score_col_name}}) -> scores_by_location
    ensemble_rank_scores |> 
      mutate(yval = 1 / {{score_col_name}}) |> 
      mutate(k=as.numeric(str_replace(model, 'ens_rank-', replacement = ''))) -> ensemble_rank_scores
    individual_rank_scores |> 
      mutate(yval = 1 / {{score_col_name}}) |> 
      mutate(k=as.numeric(str_replace(model, 'ind_rank-', replacement = ''))) -> individual_rank_scores
    
    ## Get rt ensemble value for plotting
    scores_by_location |> 
      filter(rt_ensemble_mod)  -> rt_ensemble_vals
    
    ## Get baseline value for plotting
    scores_by_location |> 
      filter(baseline_mod) -> baseline_vals
    
    ## Get ensemble rank values for plotting
    ensemble_rank_scores |> 
      filter(k==4)  -> ensemble_rank_vals
    
    ## Get individual rank values for plotting
    individual_rank_scores |> 
      filter(k==4)  -> individual_rank_vals
    
    ## Get random ensemble values for plotting
    scores_by_location |> 
      filter(!rt_ensemble_mod, k == 4) |> 
      group_by(time_period, location) |> 
      summarize(min_val = min(yval),
                mean_val = mean(yval),
                max_val = max(yval)) -> random_ensemble_vals
    
    
    random_ensemble_vals |> 
      select(time_period, location, yval = mean_val) |> 
      mutate(model = 'Random') |> 
      bind_rows(rt_ensemble_vals |> 
                  select(time_period, location, yval = yval) |> 
                  mutate(model = 'Published ensemble'),
                baseline_vals |> 
                  select(time_period, location, yval = yval) |> 
                  mutate(model = 'Baseline'),
                ensemble_rank_vals |> 
                  select(time_period, location, yval = yval) |> 
                  mutate(model = 'Ensemble rank'),
                individual_rank_vals |> 
                  select(time_period, location, yval = yval) |> 
                  mutate(model = 'Individual rank')) |> 
      ungroup() |> 
      filter(time_period == 'Test') |> 
      mutate(location = factor(location, levels = c(paste0('HHS Region ', 1:10),
                                                    'US National'))) |> 
      mutate(model = factor(model, levels = model_colors$model)) |> 
      ggplot(aes(location, yval, fill = model)) +
      geom_col(position = position_dodge()) +
      labs(x = NULL, y = score_yname, fill = NULL) +
      background_grid(major = 'y') +
      scale_fill_manual(values = model_colors$color)  +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    ensemble_rank_scores <- ensemble_rank_scores$location
    individual_rank_scores <- individual_rank_scores$location
    
    scores_by_location |> 
      mutate(yval = {{score_col_name}}) -> scores_by_location
    ensemble_rank_scores |> 
      mutate(yval = {{score_col_name}})  -> ensemble_rank_scores
    individual_rank_scores |> 
      mutate(yval = {{score_col_name}})  -> individual_rank_scores
    
    ## Get rt ensemble value for plotting
    scores_by_location |> 
      filter(rt_ensemble_mod)  -> rt_ensemble_vals
    
    ## Get baseline value for plotting
    scores_by_location |> 
      filter(baseline_mod) -> baseline_vals
    
    ## Get ensemble rank values for plotting
    ensemble_rank_scores |> 
      filter(k==4)  -> ensemble_rank_vals
    
    ## Get individual rank values for plotting
    individual_rank_scores |> 
      filter(k==4)  -> individual_rank_vals
    
    ## Get random ensemble values for plotting
    scores_by_location |> 
      filter(!rt_ensemble_mod, k == 4) |> 
      group_by(time_period, location) |> 
      summarize(min_val = min(yval),
                mean_val = mean(yval),
                max_val = max(yval)) -> random_ensemble_vals
    
    load('raw-data/case-data.rda')
    locations <- truth_data |> 
      filter(geo_type == 'state') |> 
      distinct(location, location_name, abbreviation,population)
    locations_to_exclude <- c('60', '66', '69', '74')
    # browser()
    random_ensemble_vals |> 
      select(time_period, location, yval = mean_val) |> 
      mutate(model = 'Random') |> 
      bind_rows(rt_ensemble_vals |> 
                  select(time_period, location, yval = yval) |> 
                  mutate(model = 'Published ensemble'),
                baseline_vals |> 
                  select(time_period, location, yval = yval) |> 
                  mutate(model = 'Baseline'),
                ensemble_rank_vals |> 
                  select(time_period, location, yval = yval) |> 
                  mutate(model = 'Ensemble rank'),
                individual_rank_vals |> 
                  select(time_period, location, yval = yval) |> 
                  mutate(model = 'Individual rank')) |> 
      ungroup() |> 
      filter(time_period == 'test', !(location%in% locations_to_exclude)) |> 
      left_join(locations, by = 'location') |> 
      # mutate(abbreviation = fct_reorder(abbreviation, population)) |> 
      # mutate(abbreviation = fct_reorder(abbreviation, as.numeric(abbreviation))) |>
      mutate(model = factor(model, levels = model_colors$model)) |> 
      ggplot(aes(abbreviation, yval/population, color = model, group = model)) +
      geom_line() +
      geom_point() +
      labs(x = NULL, y = score_yname, color = NULL,
           title = truth_yname) +
      background_grid(major = 'xy', minor = '') +
      scale_color_manual(values = model_colors$color) +
      guides(color = guide_legend(override.aes = list(linewidth = 5))) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }
}


## Multiyear overall plot
plot_location_performance(summary_file_path = 'processed-data/multiyear-score-summaries.rda', 
                         score_col_name = score,
                         truth_yname = 'ILI %',
                         score_yname = 'Forecast score') -> my_location
my_location
save_plot( filename = 'figs/my_location.png', 
           my_location,
           base_height = 6,
           base_asp = 1.8,
           bg = 'white')

## Covid-19 case overall plot
plot_location_performance(summary_file_path = 'processed-data/case-score-summaries.rda', 
                         score_col_name = avg_wis,
                         truth_yname = 'COVID-19 cases',
                         score_yname = 'Population Standardized WIS') -> case_location
case_location

## Covid-19 hospital admissions overall plot
plot_location_performance(summary_file_path = 'processed-data/hosp2-score-summaries.rda', 
                         score_col_name = avg_wis,
                         truth_yname = 'COVID-19 admissions',
                         score_yname = 'Population Standardized WIS') -> covidadmit_location
covidadmit_location


## Covid-19 mortality overall plot
plot_location_performance(summary_file_path = 'processed-data/death-score-summaries.rda', 
                         score_col_name = avg_wis,
                         truth_yname = 'COVID-19 mortality',
                         score_yname = 'Population Standardized WIS') -> deaths_location
deaths_location


## Flu admissions overall plot
plot_location_performance(summary_file_path = 'processed-data/flu_hosp-score-summaries.rda', 
                         score_col_name = avg_wis,
                         truth_yname = 'Influenza admissions',
                         score_yname = 'Population Standardized WIS') -> fluadmits_location
fluadmits_location



## Put together into a single figure.

panel_plot <- plot_grid(
  case_location + theme(legend.position='none'), 
  covidadmit_location + theme(legend.position='none'), 
  deaths_location + theme(legend.position='none'), 
  fluadmits_location + theme(legend.position='none'),
  nrow = 4, align = 'hv') |> plot_grid(get_legend(case_location), nrow = 1, rel_widths = c(1, .2))
panel_plot

save_plot('figs/location-summary.png', 
          panel_plot, 
          base_height = 12, 
          base_asp = 1.2,
          bg='white')


