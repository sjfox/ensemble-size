## Script to produce figure showing individual component forecasts alongside ensemble ones
library(tidyverse)
library(cowplot)
library(covidHubUtils)
library(hubEnsembles)
library(lubridate)
library(ggrepel)
theme_set(theme_cowplot())


# First gather all the raw forecasts --------------------------------------
gather_raw_forecasts <- function(file, 
                                 location_abbr){
  load(file)
  forecast_data |> 
    filter(abbreviation == location_abbr) 
}


# Mortality ---------------------------------------------------------------

list.files('raw-data/death', full = T) |> 
  map(gather_raw_forecasts, 
      location_abbr = 'MA') |> 
  bind_rows() -> raw_forecasts


# Now create the ensemble and individual rank forecasts -------------------
mod_info <- read_csv('raw-data/death-model-combination-lookup-table.csv')

load('processed-data/death-score-summaries.rda')

## Ensemble rank forecasts
ens_rank <- ensemble_rank_scores$overall |> 
  filter(k == 4) |> pull(combination_num) |> unique()

ens_rank_forecasts <- raw_forecasts %>% 
  inner_join(mod_info |> filter(combination_num == ens_rank) |> 
               select(model), by = 'model') |> 
  filter(geo_type == 'state') |> 
  build_quantile_ensemble(method = "median",
                          forecast_date = '1980-01-01',
                          model_name = 'Ensemble rank')
ens_rank_forecasts

# ## Individual rank forecasts
# ind_rank <- individual_rank_scores$overall |> 
#   filter(k == 5) |> pull(combination_num)
# 
# ind_rank_forecasts <- raw_forecasts %>% 
#   inner_join(mod_info |> filter(combination_num == ind_rank) |> 
#                select(model), by = 'model') |> 
#   filter(geo_type == 'state') |> 
#   build_quantile_ensemble(method = "median",
#                           forecast_date = '1980-01-01',
#                           model_name = 'Individual rank')


# Plot things out ---------------------------------------------------------
load('raw-data/death-data.rda')

forecast_reference_dates <- unique(raw_forecasts$reference_date)
forecast_reference_dates <- forecast_reference_dates[seq(1, length(forecast_reference_dates), by = 4)]

## Select raw forecasts to show
quantiles_to_show <- c(0.025, 0.5, 0.975)
raw_forecasts |> 
  filter(quantile %in% quantiles_to_show,
         geo_type == 'state',
         model == 'COVIDhub-4_week_ensemble',
         reference_date %in% forecast_reference_dates) |> 
  select(reference_date, target_end_date, quantile, value) |> 
  spread(quantile,value) -> rt_ensemble_forecasts_df
colnames(rt_ensemble_forecasts_df) <- c(colnames(rt_ensemble_forecasts_df)[1:2],
                                          'lo', 'mid', 'hi')
ens_rank_forecasts |> 
  mutate(reference_date = target_end_date-days(7*as.numeric(horizon))) |> 
  filter(quantile %in% quantiles_to_show,
         reference_date %in% forecast_reference_dates) |> 
  filter(reference_date > '2021-11-15') |>
  select(reference_date, target_end_date, quantile, value) |> 
  spread(quantile,value) -> ensemble_rank_forecasts_df
colnames(ensemble_rank_forecasts_df) <- c(colnames(ensemble_rank_forecasts_df)[1:2],
                                          'lo', 'mid', 'hi')


truth_data |> 
  filter(abbreviation == 'MA', geo_type=='state') |> 
  filter(target_end_date %in% 
           unique(c(raw_forecasts$reference_date, 
                    raw_forecasts$target_end_date)))  -> truth_for_plotting
  

rt_ensemble_forecasts_df |> 
  mutate(model = 'Published ensemble') |> 
  bind_rows(ensemble_rank_forecasts_df |> 
              mutate(model = 'Ensemble rank')) |> 
  ggplot(aes(target_end_date, mid, color = model, group = interaction(reference_date,model))) + 
  geom_line(data = truth_for_plotting |> 
              mutate(model = 'Data'), aes(x = target_end_date, y = value, linetype = model), color = 'black', inherit.aes=F) +
  geom_ribbon(aes(x = target_end_date, ymin = lo, ymax=hi, 
                  group = interaction(reference_date,model), fill = model),
              alpha = .4, color = NA) +
  geom_line() +
  scale_color_manual(values = c('#A16928', '#764E9F')) +
  scale_fill_manual(values = c('#A16928', '#764E9F', NA)) +
  scale_x_date(date_breaks = '6 months', date_labels = '%b-%Y') +
  background_grid(major = 'x', minor = 'x') +
  annotate('segment', x = min(raw_forecasts$reference_date), xend = ymd('2021-11-15'),
                      y = 750, yend = 750) + 
  annotate('text', x = mean(c(min(raw_forecasts$reference_date),ymd('2021-11-15'))), 
           y = 750, label = 'Training period', hjust = 'center', vjust = -0.1,size = 5) +
  theme(legend.spacing.y = unit(0.0, "cm"),
        legend.position = c(0.75,0.8)) +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 1)) +
  labs(x = NULL, y = 'COVID-19 Mortality', color = NULL, fill = NULL, linetype = NULL) -> ex_ensemble_forecast_plot
ex_ensemble_forecast_plot
save_plot('figs/ex_ensemble_forecast_plot.png', ex_ensemble_forecast_plot,
          base_height = 4, base_asp = 2.25, bg = 'white')



## Load plots for overall scores
load('figs/ggplot-objects/overall-summary-figs.rda')


plot_grid(ex_ensemble_forecast_plot,
          panel_plot, labels = 'AUTO',
          nrow = 2, rel_heights = c(0.4, 0.8)) -> combined_fig1
save_plot('figs/combined-fig-1.png', 
          combined_fig1, bg = 'white',
          base_height = 10, base_asp = 1.2)
  




  
