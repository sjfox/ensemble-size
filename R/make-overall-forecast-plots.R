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
  
  # overall_scores |> 
  #   filter(tolower(time_period) == 'test') |> 
  #   group_by(k) |> 
  #   summarize(min_val = quantile(yval, probs = 0.025),
  #             mean_val = mean(yval),
  #             max_val = quantile(yval, probs = 0.95)) -> random_ensemble_vals
  
  # browser()
  random_ensemble_vals |> 
    select(k, `All ensembles` = mean_val) |> 
    # left_join(ensemble_rank_vals, by = 'k') |> 
    # left_join(ind_rank_vals, by = 'k') |> 
    # rename(`Ensemble rank` = ens_rank,
           # `Individual rank` = ind_rank) |> 
    gather(key, value, -k) |> 
    ggplot(aes(k, value/baseline_val, color = key, fill = key)) +
      geom_ribbon(data = random_ensemble_vals |> 
                    mutate(key = 'All ensembles'),
                  aes(x = k, ymin = min_val/baseline_val, ymax = max_val/baseline_val, fill = key), 
                  alpha = .1, color = NA, inherit.aes=F)   +
      geom_line(size = .7) +
      geom_hline(data = tibble(key = c('Published ensemble'),
                               yval = c(rt_ensemble_val/baseline_val)),
                 aes(yintercept = yval, lty = key), color = '#764E9F', size = .8) +
      geom_text_repel(data = tibble(label = c('Published\nensemble'),
                                     xval = max(random_ensemble_vals$k)+0.5,
                                     yval = c(rt_ensemble_val/baseline_val)),
                       aes(x =xval, y = yval, label = label), 
                       nudge_y = 0.3, size = 6,
                       color = '#764E9F', 
                       inherit.aes = F)+
      scale_linetype_manual(values = c(2)) +
      # scale_color_manual(values = c( '#A16928', '#2887a1','black')) +
      # scale_fill_manual(values = c('white', 'white', 'black')) +
      scale_color_manual(values = c('black')) +
      scale_fill_manual(values = c('black')) +
        coord_cartesian(ylim = c(min(rt_ensemble_val/baseline_val, 
                                     (random_ensemble_vals |> 
                                       mutate(key = 'All ensembles') |> 
                                       pull(min_val))/baseline_val)*.95, 
                                 baseline_val/baseline_val*1.1)) +
        # background_grid(major ='xy') +
        labs(x = 'Number of included models', 
             y = 'Relative forecast score', 
             title = plot_title, 
             color = NULL, 
             lty = NULL,
             fill = NULL) +
        theme(legend.position = 'none') +
        scale_x_continuous(breaks = 1:max(random_ensemble_vals$k)) +
        NULL
}
# '#E58606','#5D69B1','#52BCA3','#99C945','#CC61B0','#24796C','#DAA51B','#2F8AC4''#764E9F',#ED645A,#CC3A8E,#A5AA99


## Multiyear overall plot
plot_overall_performance(summary_file_path = 'processed-data/multiyear-score-summaries.rda', 
                         score_col_name = score,
                         plot_title = 'ILI %') -> my_overall
my_overall
save_plot( filename = 'figs/multiyear-overall.png', 
          my_overall,
          base_height = 5,
          base_asp = 1.5,
          bg = 'white')

## Covid-19 case overall plot
plot_overall_performance(summary_file_path = 'processed-data/case-score-summaries.rda', 
                         score_col_name = avg_wis,
                         plot_title = 'COVID-19 cases') -> covidcase_overall
covidcase_overall
save_plot( filename = 'figs/covidcase-overall.png', 
           covidcase_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')

## Covid-19 hospital admissions overall plot
plot_overall_performance(summary_file_path = 'processed-data/hosp2-score-summaries.rda', 
                         score_col_name = avg_wis,
                         plot_title = 'COVID-19 admissions') -> covidadmits_overall
covidadmits_overall
save_plot( filename = 'figs/covidadmits_overall-overall.png', 
           covidadmits_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')

## Covid-19 mortality overall plot
plot_overall_performance(summary_file_path = 'processed-data/death-score-summaries.rda', 
                         score_col_name = avg_wis,
                         plot_title = 'COVID-19 mortality') -> coviddeaths_overall
coviddeaths_overall
save_plot( filename = 'figs/coviddeaths_overall_overall-overall.png', 
           coviddeaths_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')

## Flu admissions overall plot
plot_overall_performance(summary_file_path = 'processed-data/flu_hosp-score-summaries.rda', 
                         score_col_name = avg_wis,
                         plot_title = 'Influenza admissions') -> fluadmits_overall
fluadmits_overall
save_plot( filename = 'figs/fluadmits_overall_overall-overall.png', 
           fluadmits_overall,
           base_height = 5,
           base_asp = 1.5,
           bg = 'white')


## Put together into a single figure.

covidcase_overall$layers[[4]] <-NULL
covidadmits_overall$layers[[4]] <-NULL
coviddeaths_overall$layers[[4]] <-NULL
fluadmits_overall$layers[[4]] <-NULL

panel_plot <- plot_grid(
                      covidcase_overall, 
                      covidadmits_overall, 
                      coviddeaths_overall, 
                      fluadmits_overall,
                      my_overall + 
                        scale_x_continuous(breaks = seq(1, 23, by = 2)),
                      nrow = 2, align = 'hv') 

panel_plot

save_plot('figs/overall-summary-fig.png', 
          panel_plot, 
          base_height = 7, 
          base_asp = 1.65,
          bg='white')

save(my_overall,
     covidcase_overall, 
     covidadmits_overall, 
     coviddeaths_overall, 
     fluadmits_overall,
     panel_plot,
     file = 'figs/ggplot-objects/overall-summary-figs.rda')
