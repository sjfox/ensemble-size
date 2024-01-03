library(tidyverse)
library(cowplot)
library(ggrepel)
library(MMWRweek)
theme_set(theme_cowplot())


plot_ts_performance <- function(summary_file_path,
                                score_col_name,
                                truth_data_file_paths,
                                truth_yname,
                                score_yname){
  require(MMWRweek)
  
  load(summary_file_path)
  # 
  # browser()
  
  model_colors <- tibble(model = c('Baseline', 'Ensemble rank', 'Individual rank', 'Random', 'Real-time ensemble'),
                         color = c('darkgrey', '#A16928', '#2887a1', 'black', '#764E9F'))
  
  if(grepl(pattern = 'multiyear', x = summary_file_path, fixed = T)){
    ensemble_rank_scores <- ensemble_rank_scores$score_by_date
    individual_rank_scores <- individual_rank_scores$score_by_date
    
    scores_by_date |> 
      mutate(yval = 1 / {{score_col_name}}) -> scores_by_date
    ensemble_rank_scores |> 
      mutate(yval = 1 / {{score_col_name}}) |> 
      mutate(k=as.numeric(str_replace(model, 'ens_rank-', replacement = ''))) -> ensemble_rank_scores
    individual_rank_scores |> 
      mutate(yval = 1 / {{score_col_name}}) |> 
      mutate(k=as.numeric(str_replace(model, 'ind_rank-', replacement = ''))) -> individual_rank_scores
    
    ## Get rt ensemble value for plotting
    scores_by_date |> 
      filter(rt_ensemble_mod)  -> rt_ensemble_vals
    
    ## Get baseline value for plotting
    scores_by_date |> 
      filter(baseline_mod) -> baseline_vals
    
    ## Get ensemble rank values for plotting
    ensemble_rank_scores |> 
      filter(k==4)  -> ensemble_rank_vals
    
    ## Get individual rank values for plotting
    individual_rank_scores |> 
      filter(k==4)  -> individual_rank_vals
    
    ## Get random ensemble values for plotting
    scores_by_date |> 
      filter(!rt_ensemble_mod, k == 4) |> 
      group_by(forecast_week, season) |> 
      summarize(min_val = min(yval),
                mean_val = mean(yval),
                max_val = max(yval)) -> random_ensemble_vals
    
    target_bounds <- read_csv('raw-data/all-target-bounds.csv')
    train_rect <- tibble(ymin = 0,
                         ymax = Inf,
                         xmin = structure(-Inf, class = "Date"), 
                         xmax = ymd('2014-07-01'))

    ili <- read_csv(truth_data_file_paths)
    ili <- ili |> 
      filter(region == 'nat') |> 
      select(epiweek, target = `final-observed-wili`) |>
      mutate(year = substr(epiweek, 1, 4),
             week = substr(epiweek, 5, 6)) |> 
      mutate(date = MMWRweek2Date(as.numeric(year), as.numeric(week)))
      
    min_date <- min(ili$date)
    max_date <- max(ili$date)
    ili |> 
      ggplot(aes(date, target)) + 
      geom_rect(data = train_rect,
                aes(ymin=ymin, ymax=ymax,xmin=xmin,xmax=xmax), 
                alpha = .1, fill = 'black', color = NA, inherit.aes=F) +
        geom_line() +
        labs(x = NULL, y = truth_yname) +
        scale_x_date(date_breaks = 'year', date_labels = '%Y', limits = ymd(c(min_date,max_date))) +
        background_grid(major = 'xy', minor = 'xy') -> ts_truth_scores
        
    random_ensemble_vals |> 
      select(season, forecast_week, yval = mean_val) |> 
      mutate(model = 'Random') |> 
      bind_rows(rt_ensemble_vals |> 
                  select(season, forecast_week, yval = yval) |> 
                  mutate(model = 'Real-time ensemble'),
                baseline_vals |> 
                  select(season, forecast_week, yval = yval) |> 
                  mutate(model = 'Baseline'),
                ensemble_rank_vals |> 
                  select(season, forecast_week, yval = yval) |> 
                  mutate(model = 'Ensemble rank'),
                individual_rank_vals |> 
                  select(season, forecast_week, yval = yval) |> 
                  mutate(model = 'Individual rank')) |> 
      ungroup() |> 
      mutate(season_start_year = substr(season, start = 1, stop = 4)) |> 
      mutate(season_week = ifelse(forecast_week > 39,
                                  forecast_week,
                                  ifelse(season_start_year == 2014,
                                         forecast_week + 53,
                                         forecast_week + 52))) |> 
      inner_join(target_bounds |> 
                   filter(Location == 'US National', Target == '1 wk ahead'), 
                 by = c('season' = 'Season')) %>% 
      filter(season_week >= start_week_seq & season_week <= end_week_seq) |> 
      mutate(actual_year = ifelse(forecast_week > 35, as.numeric(season_start_year), as.numeric(season_start_year)+1)) |> 
      mutate(date = MMWRweek2Date(as.numeric(actual_year), forecast_week))  |> 
      mutate(yval = ifelse(date < '2014-07-01' & model %in% c('Ensemble rank', 'Individual rank'), NA, yval)) |> 
      ggplot(aes(date, yval, color = model, group = interaction(model, season))) +
        geom_rect(data = train_rect,
                  aes(ymin=ymin, ymax=ymax,xmin=xmin,xmax=xmax), 
                  alpha = .1, fill = 'black', color = NA, inherit.aes=F) +
        geom_line() +
        labs(x = NULL, y = score_yname, color = NULL) +
        background_grid(major = 'xy', minor = 'xy') +
        scale_color_manual(values = model_colors$color) +
        scale_y_log10() +
      scale_x_date(date_breaks = 'year', date_labels = '%Y', limits = ymd(c(min_date,max_date))) +
        theme(legend.position = 'bottom',
              legend.justification = c(0.5,0.5)) +
        guides(color = guide_legend(override.aes = list(linewidth = 5))) -> ts_wis_scores
    
    plot_grid(ts_truth_scores, ts_wis_scores, nrow = 2, align = 'v', rel_heights = c(1,1.1))
  } else {
    ensemble_rank_scores <- ensemble_rank_scores$date
    individual_rank_scores <- individual_rank_scores$date
    
    scores_by_date |> 
      mutate(yval = {{score_col_name}}) -> scores_by_date
    ensemble_rank_scores |> 
      mutate(yval = {{score_col_name}})  -> ensemble_rank_scores
    individual_rank_scores |> 
      mutate(yval = {{score_col_name}})  -> individual_rank_scores
    
    ## Get rt ensemble value for plotting
    scores_by_date |> 
      filter(rt_ensemble_mod)  -> rt_ensemble_vals
    
    ## Get baseline value for plotting
    scores_by_date |> 
      filter(baseline_mod) -> baseline_vals
    
    ## Get ensemble rank values for plotting
    ensemble_rank_scores |> 
      filter(k==4)  -> ensemble_rank_vals
    
    ## Get individual rank values for plotting
    individual_rank_scores |> 
      filter(k==4)  -> individual_rank_vals
    
    ## Get random ensemble values for plotting
    scores_by_date |> 
      filter(!rt_ensemble_mod, k == 4) |> 
      group_by(forecast_date) |> 
      summarize(min_val = min(yval),
                mean_val = mean(yval),
                max_val = max(yval)) -> random_ensemble_vals
    
    load(truth_data_file_paths)  
    
    rt_ensemble_vals |> 
      filter(time_period == 'train') |> 
      pull(forecast_date) |> max() |> ymd() -> max_train_period
    rt_ensemble_vals |> 
      filter(time_period == 'train') |> 
      pull(forecast_date) |> min() |> ymd() -> min_train_period
    
    rt_ensemble_vals |> 
      filter(time_period == 'test') |> 
      pull(forecast_date) |> max() |> ymd() -> max_test_period
    
    train_rect <- tibble(ymin = 0,
                         ymax = Inf,
                         xmin = structure(-Inf, class = "Date"), 
                         xmax = max_train_period)
    
    
    truth_data <- truth_data |> 
      filter(location == 'US') 

    truth_data |> 
      ggplot(aes(target_end_date, value)) + 
      geom_rect(data = train_rect,
                aes(ymin=ymin, ymax=ymax,xmin=xmin,xmax=xmax), 
                alpha = .1, fill = 'black', color = NA, inherit.aes=F) +
      geom_line() +
      labs(x = NULL, y = truth_yname) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_breaks = '2 months', date_labels = '%b-%y', 
                   limits = ymd(c(min_train_period, max_test_period))) +
      background_grid(major = 'xy', minor = 'xy') -> ts_truth_scores
    
    random_ensemble_vals |> 
      select(forecast_date, yval = mean_val) |> 
      mutate(model = 'Random') |> 
      bind_rows(rt_ensemble_vals |> 
                  select(forecast_date, yval = yval) |> 
                  mutate(model = 'Real-time ensemble'),
                baseline_vals |> 
                  select(forecast_date, yval = yval) |> 
                  mutate(model = 'Baseline'),
                ensemble_rank_vals |> 
                  select(forecast_date, yval = yval) |> 
                  mutate(model = 'Ensemble rank'),
                individual_rank_vals |> 
                  select(forecast_date, yval = yval) |> 
                  mutate(model = 'Individual rank')) |> 
      ungroup() |> 
      mutate(time_period = ifelse(forecast_date <= max_train_period, 'train', 'test')) |> 
      mutate(yval = ifelse(time_period == 'train' & model %in% c('Ensemble rank', 'Individual rank'), NA, yval)) |> 
      ggplot(aes(forecast_date, yval, color = model, group = interaction(time_period, model))) +
      geom_rect(data = train_rect,
                aes(ymin=ymin, ymax=ymax,xmin=xmin,xmax=xmax), 
                alpha = .1, fill = 'black', color = NA, inherit.aes=F) +
      geom_line() +
      labs(x = NULL, y = score_yname, color = NULL) +
      background_grid(major = 'xy', minor = 'xy') +
      scale_color_manual(values = model_colors$color) +
      scale_y_log10() +
      scale_x_date(date_breaks = '2 months', date_labels = '%b-%y', 
                   limits = ymd(c(min_train_period, max_test_period))) +
      theme(legend.position = 'bottom',
            legend.justification = c(0.5,0.5)) +
      guides(color = guide_legend(override.aes = list(linewidth = 5))) -> ts_wis_scores
    
    plot_grid(ts_truth_scores, ts_wis_scores, nrow = 2, align = 'v', rel_heights = c(1,1.1))
  }
}


## Multiyear overall plot
plot_ts_performance(summary_file_path = 'processed-data/multiyear-score-summaries.rda', 
                      score_col_name = score,
                      truth_data_file_path = 'raw-data/my-ili_data.csv',
                      truth_yname = 'ILI %',
                      score_yname = 'Forecast score') -> my_ts_dates
my_ts_dates
save_plot( filename = 'figs/my_ts_dates.png', 
           my_ts_dates,
           base_height = 6,
           base_asp = 1.5,
           bg = 'white')

## Covid-19 case overall plot
plot_ts_performance(summary_file_path = 'processed-data/case-score-summaries.rda', 
                     score_col_name = avg_wis,
                     truth_data_file_path = 'raw-data/case-data.rda',
                     truth_yname = 'COVID-19 cases',
                     score_yname = 'WIS') -> case_ts_dates
case_ts_dates
save_plot( filename = 'figs/case_ts_dates.png', 
           case_ts_dates,
           base_height = 6,
           base_asp = 1.5,
           bg = 'white')

## Covid-19 hospital admissions overall plot
plot_ts_performance(summary_file_path = 'processed-data/hosp2-score-summaries.rda', 
                    score_col_name = avg_wis,
                    truth_data_file_path = 'raw-data/hosp-data.rda',
                    truth_yname = 'COVID-19 admissions',
                    score_yname = 'WIS') -> covidadmits_ts_dates
covidadmits_ts_dates
save_plot( filename = 'figs/covidadmits_ts_dates.png', 
           covidadmits_ts_dates,
           base_height = 6,
           base_asp = 1.5,
           bg = 'white')

## Covid-19 mortality overall plot
plot_ts_performance(summary_file_path = 'processed-data/death-score-summaries.rda', 
                         score_col_name = avg_wis,
                    truth_data_file_path = 'raw-data/death-data.rda',
                    truth_yname = 'COVID-19 mortality',
                    score_yname = 'WIS') -> deaths_ts_dates
deaths_ts_dates
save_plot( filename = 'figs/deaths_ts_dates.png', 
           deaths_ts_dates,
           base_height = 6,
           base_asp = 1.5,
           bg = 'white')

## Flu admissions overall plot
plot_ts_performance(summary_file_path = 'processed-data/flu_hosp-score-summaries.rda', 
                         score_col_name = avg_wis,
                    truth_data_file_path = 'raw-data/flu_hosp-data.rda',
                    truth_yname = 'Influenza admissions',
                    score_yname = 'WIS') -> fluadmits_ts_dates
fluadmits_ts_dates
save_plot( filename = 'figs/fluadmits_ts_dates.png', 
           fluadmits_ts_dates,
           base_height = 6,
           base_asp = 1.5,
           bg = 'white')

