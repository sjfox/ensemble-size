# Functions for creating and scoring ensembles
get_ensemble_forecast <- function(combination_num,
                                  models_included, 
                                  forecast_df,
                                  forecast_date){
  build_quantile_ensemble(forecast_df %>% 
                            filter(model %in% models_included$model), 
                          method = "median",
                          forecast_date = forecast_date,
                          model_name = paste0("ensemble-", combination_num),
                          location_data = hub_locations)
}

get_all_ensemble_forecasts <- function(forecast_data,
                                       forecast_date,
                                       model_combination_lookup){
  ## Function gets all ensemble forecasts for 
  ## all model combinations for a specific date and metric
  
  ## Create ensemble for all model combinations
  model_combination_lookup %>% 
    nest(data = c('k', 'model')) %>% 
    mutate(ensemble_forecast = map2(combination_num,
                                    data,
                                    .f = get_ensemble_forecast,
                                    forecast_df = forecast_data,
                                    forecast_date = forecast_date)) %>% 
    select(-data) %>% 
    unnest(ensemble_forecast) -> ensemble_forecasts
  
  return(ensemble_forecasts)

}
