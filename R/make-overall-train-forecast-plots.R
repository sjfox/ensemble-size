library(tidyverse)
library(cowplot)
library(ggrepel)
theme_set(theme_cowplot())


plot_overall_performance <- function(summary_file_path,
                                     score_col_name,
                                     plot_title){
  
  load(summary_file_path)
  # 
  # browser()
  
  if(grepl(pattern = 'multiyear', x = summary_file_path, fixed = T)){
    ensemble_rank_overall <- ensemble_rank_scores$overall_score
    individual_rank_overall <- individual_rank_scores$overall_score
    
    overall_scores |> 
      mutate(yval = 1 / {{score_col_name}}) -> overall_scores
    ensemble_rank_overall |> 
      mutate(yval = 1 / {{score_col_name}}) |> 
      mutate(k=as.numeric(str_replace(model, 'ens_rank-', replacement = ''))) -> ensemble_rank_overall
    individual_rank_overall |> 
      mutate(yval = 1 / {{score_col_name}}) |> 
      mutate(k=as.numeric(str_replace(model, 'ind_rank-', replacement = ''))) -> individual_rank_overall
  } else {
    ensemble_rank_overall <- ensemble_rank_scores$overall
    individual_rank_overall <- individual_rank_scores$overall
    overall_scores |> 
      mutate(yval = {{score_col_name}}) -> overall_scores
    ensemble_rank_overall |> 
      mutate(yval = {{score_col_name}}) -> ensemble_rank_overall
    individual_rank_overall |> 
      mutate(yval = {{score_col_name}}) -> individual_rank_overall
  }
  
  ## Get rt ensemble value for plotting
  overall_scores |> 
    filter(rt_ensemble_mod, tolower(time_period) == 'train') |> 
    pull(yval) -> rt_ensemble_val
  
  ## Get baseline value for plotting
  overall_scores |> 
    filter(baseline_mod, tolower(time_period) == 'train') |> 
    pull(yval) -> baseline_val
  
  ## Get ensemble rank values for plotting
  ensemble_rank_overall |> 
    filter(tolower(time_period) == 'train') |> 
    select(k, ens_rank = yval) -> ensemble_rank_vals
  
  ## Get individual rank values for plotting
  individual_rank_overall |> 
    filter(tolower(time_period) == 'train') |> 
    select(k, ind_rank = yval) -> ind_rank_vals
  
  ## Get random ensemble values for plotting
  overall_scores |> 
    filter(tolower(time_period) == 'train') |> 
    group_by(k) |> 
    summarize(min_val = min(yval),
              mean_val = mean(yval),
              max_val = max(yval)) -> random_ensemble_vals
  
  random_ensemble_vals |> 
    select(k, Random = mean_val) |> 
    left_join(ensemble_rank_vals, by = 'k') |> 
    left_join(ind_rank_vals, by = 'k') |> 
    rename(`Ensemble rank` = ens_rank,
           `Individual rank` = ind_rank) |> 
    gather(key, value, -k) |> 
    ggplot(aes(k, value/baseline_val, color = key, fill = key)) +
    geom_ribbon(data = random_ensemble_vals |> 
                  mutate(key = 'Random'),
                aes(x = k, ymin = min_val/baseline_val, ymax = max_val/baseline_val, fill = key), 
                alpha = .1, color = NA, inherit.aes=F)   +
    geom_line() +
    scale_color_manual(values = c('#A16928', '#2887a1', 'black')) +
    scale_fill_manual(values = c('white', 'white', 'black')) +
    geom_label_repel(data = tibble(x = Inf,
                                   y = baseline_val/baseline_val,
                                   label = 'Baseline'),
                     aes(x=x, y=y, label=label),
                     color = 'grey30',
                     nudge_y = baseline_val/baseline_val*.15,
                     inherit.aes = FALSE) +
    geom_hline(yintercept = rt_ensemble_val/baseline_val, lty = 2, color = 'grey30') +
    geom_hline(yintercept = baseline_val/baseline_val, lty = 3, color = 'grey30') +
    geom_label_repel(data = tibble(x = Inf,
                                   y = rt_ensemble_val,
                                   label = 'RT Ensemble'),
                     aes(x=x, y=y/baseline_val, label=label),
                     color = 'grey30',
                     nudge_y = rt_ensemble_val/baseline_val*.15,
                     inherit.aes = FALSE) +
    coord_cartesian(ylim = c(min(random_ensemble_vals$min_val/baseline_val)*.975, baseline_val/baseline_val*1.1)) +
    # background_grid(major ='xy') +
    labs(x = 'Number of included models', 
         y = 'Relative forecast score', 
         title = plot_title, 
         color = NULL, 
         fill = NULL) +
    guides(color = guide_legend(override.aes = list(linewidth = 1))) +
    scale_x_continuous(breaks = 1:max(random_ensemble_vals$k)) +
    NULL
  
}


## Multiyear overall plot
plot_overall_performance(summary_file_path = 'processed-data/multiyear-score-summaries.rda', 
                         score_col_name = score,
                         plot_title = 'Multiyear ILI %') -> my_overall
my_overall
save_plot( filename = 'figs/multiyear-train-overall.png', 
           my_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')

## Covid-19 case overall plot
plot_overall_performance(summary_file_path = 'processed-data/case-score-summaries.rda', 
                         score_col_name = avg_wis,
                         plot_title = 'COVID-19 cases') -> covidcase_overall
covidcase_overall
save_plot( filename = 'figs/covidcase-train-overall.png', 
           covidcase_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')

## Covid-19 hospital admissions overall plot
plot_overall_performance(summary_file_path = 'processed-data/hosp2-score-summaries.rda', 
                         score_col_name = avg_wis,
                         plot_title = 'COVID-19 admissions') -> covidadmits_overall
covidadmits_overall
save_plot( filename = 'figs/covidadmits-train-overall.png', 
           covidadmits_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')

## Covid-19 mortality overall plot
plot_overall_performance(summary_file_path = 'processed-data/death-score-summaries.rda', 
                         score_col_name = avg_wis,
                         plot_title = 'COVID-19 mortality') -> coviddeaths_overall
coviddeaths_overall
save_plot( filename = 'figs/coviddeaths-train-overall.png', 
           coviddeaths_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')

## Flu admissions overall plot
plot_overall_performance(summary_file_path = 'processed-data/flu_hosp-score-summaries.rda', 
                         score_col_name = avg_wis,
                         plot_title = 'Influenza admissions') -> fluadmits_overall
fluadmits_overall
save_plot( filename = 'figs/fluadmits-train-overall.png', 
           fluadmits_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')


## Put together into a single figure.

panel_plot <- plot_grid(my_overall + 
                          theme(legend.position='none') +
                          scale_x_continuous(breaks = seq(1, 23, by = 2)),
                        covidcase_overall + theme(legend.position='none'), 
                        covidadmits_overall + theme(legend.position='none'), 
                        coviddeaths_overall + theme(legend.position='none'), 
                        fluadmits_overall + theme(legend.position='none'),
                        nrow = 2, align = 'hv') +
  draw_plot(get_legend(my_overall + 
                         guides(color = guide_legend(label.theme = element_text(size = 16),
                                                     keywidth = 2, 
                                                     keyheight = 2,
                                                     override.aes = list(linewidth = 2)))), 
            x = .75, y = -.2)
panel_plot

save_plot('figs/train-overall-summary-fig.png', 
          panel_plot, 
          base_height = 7, 
          base_asp = 1.6,
          bg='white')

save(my_overall,
     covidcase_overall, 
     covidadmits_overall, 
     coviddeaths_overall, 
     fluadmits_overall,
     panel_plot,
     file = 'figs/ggplot-objects/train-overall-summary-figs.rda')



