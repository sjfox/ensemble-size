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

mod_info |> 
  filter(k == 10) |> 
  pull(combination_num) |> unique() -> full_ensemble_num


ens_forecast <- raw_forecasts %>% 
  inner_join(mod_info |> filter(combination_num == full_ensemble_num) |> 
               select(model), by = 'model') |> 
  filter(geo_type == 'state') |> 
  build_quantile_ensemble(method = "median",
                          forecast_date = '1980-01-01',
                          model_name = 'Ensemble')
ens_forecast


# Plot things out ---------------------------------------------------------
load('raw-data/death-data.rda')

forecast_reference_dates <- unique(raw_forecasts$reference_date)
forecast_reference_dates <- forecast_reference_dates[seq(1, length(forecast_reference_dates), by = 4)]

## Select raw forecasts to show
quantiles_to_show <- c(0.025, 0.5, 0.975)
raw_forecasts |> 
  filter(quantile %in% quantiles_to_show,
         geo_type == 'state',
         reference_date %in% forecast_reference_dates) |> 
  select(model, reference_date, target_end_date, quantile, value) |> 
  spread(quantile, value) -> raw_forecasts_df
colnames(raw_forecasts_df) <- c(colnames(raw_forecasts_df)[1:3],
                                        'lo', 'mid', 'hi')
ens_forecast |> 
  mutate(reference_date = target_end_date-days(7*as.numeric(horizon))) |> 
  filter(quantile %in% quantiles_to_show,
         reference_date %in% forecast_reference_dates) |> 
  # filter(reference_date > '2021-11-15') |>
  select(reference_date, target_end_date, quantile, value) |> 
  spread(quantile,value) -> ensemble_forecasts_df
colnames(ensemble_forecasts_df) <- c(colnames(ensemble_forecasts_df)[1:2],
                                          'lo', 'mid', 'hi')


truth_data |> 
  filter(abbreviation == 'MA', geo_type=='state') |> 
  filter(target_end_date %in% 
           unique(c(raw_forecasts$reference_date, 
                    raw_forecasts$target_end_date))) |> 
  filter(target_end_date > '2021-11-15') -> truth_for_plotting
truth_for_plotting |> tail()


raw_forecasts_df |> 
  filter(reference_date > '2021-11-15') |>
  ggplot(aes(target_end_date, mid, color = model, fill = model, group = interaction(reference_date,model))) + 
  geom_point(data = truth_for_plotting, aes(x = target_end_date, y = value), color = 'black', inherit.aes=F) +
    # geom_ribbon(aes(x = target_end_date, ymin = lo, ymax=hi),
                # alpha = .4, color = NA)  +
    geom_line() +
    scale_x_date(date_breaks = '4 months', date_labels = '%b-%y') +
    background_grid(major = 'x', minor = 'x') +
    coord_cartesian(ylim = c(0, 1.2*max(truth_for_plotting$value))) +
    scale_color_viridis_d() +
    theme(legend.position = 'none') +
    labs(x = NULL, y = 'COVID-19 Mortality', color = NULL, fill = NULL, linetype = NULL, title = 'Individual median forecasts') -> all_forecast_plot
all_forecast_plot

ensemble_forecasts_df |> 
  filter(reference_date > '2021-11-15') |>
  ggplot(aes(target_end_date, mid, group = interaction(reference_date))) + 
    geom_point(data = truth_for_plotting, aes(x = target_end_date, y = value), color = 'black', inherit.aes=F) +
    geom_ribbon(aes(x = target_end_date, ymin = lo, ymax=hi,
                    group = interaction(reference_date)), fill = '#cb7086',
                alpha = .4, color = NA) +
    geom_line(color = '#cb7086') +
    scale_x_date(date_breaks = '4 months', date_labels = '%b-%y') +
    background_grid(major = 'x', minor = 'x') +
    coord_cartesian(ylim = c(0, 1.2*max(truth_for_plotting$value))) +
    labs(x = NULL, y = 'COVID-19 Mortality', color = NULL, fill = NULL, linetype = NULL, title = 'Ensemble quantile forecast') ->ensemble_forecast_plot
ensemble_forecast_plot


# Make horizontal plot ----------------------------------------------------
plot_grid(all_forecast_plot, ensemble_forecast_plot) -> ensemble_process_horizontal
ensemble_process_horizontal

save_plot('figs/ensemble_process_horizontal.png', ensemble_process_horizontal,
          base_height = 4, base_asp = 4, bg = 'white')

# Make vertical plot ----------------------------------------------------
plot_grid(all_forecast_plot, ensemble_forecast_plot, nrow=2) -> ensemble_process_vertical
ensemble_process_vertical

save_plot('figs/ensemble_process_vertical.png', ensemble_process_vertical,
          base_height = 6, base_asp = 1.3, bg = 'white')


## Load plots for overall scores
load('figs/ggplot-objects/overall-summary-figs.rda')




plot_grid(plot_grid(all_forecast_plot, ensemble_forecast_plot),
          panel_plot, labels = list('A', 'B'),
          nrow = 2, rel_heights = c(0.4, 0.8)) -> combined_fig1
save_plot('figs/new-combined-fig-1.png', 
          combined_fig1, bg = 'white',
          base_height = 10, base_asp = 1.2)






