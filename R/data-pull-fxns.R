## Functions to use

create_folders <- function(raw_forecast_loc, 
                           ensemble_base_folder){
  ## Creates directory structure for saving raw and processed data
  ## Specific to analysis
  
  if(!dir.exists(raw_forecast_loc)){
    dir.create(raw_forecast_loc)
  }
  if(!dir.exists(ensemble_base_folder)){
    dir.create(ensemble_base_folder)
  }
  if(!dir.exists(paste0(ensemble_base_folder, 'ensembles'))){
    dir.create(paste0(ensemble_base_folder, 'ensembles'))
  }
  if(!dir.exists(paste0(ensemble_base_folder, 'ensemble-scores'))){
    dir.create(paste0(ensemble_base_folder, 'ensemble-scores'))
  }
  if(!dir.exists(paste0(ensemble_base_folder, 'model-scores'))){
    dir.create(paste0(ensemble_base_folder, 'model-scores'))
  }
}


download_analysis_forecasts <- function(forecast_dates,
                                        models,
                                        forecast_target_names,
                                        forecast_rel_horizons,
                                        raw_forecast_loc,
                                        forecast_hub = 'US',
                                        source = 'zoltar',
                                        ...){
  for(cur_date in forecast_dates){
    print(cur_date)
    
    forecast_data <- load_forecasts(
      models = models,
      targets = forecast_target_names,
      dates = ymd(cur_date),
      date_window_size = 2,
      locations = NULL,
      hub = forecast_hub,
      types = "quantile",
      source = source,
      verbose=F,
      ...) %>% 
      filter(!is.na(temporal_resolution))
    
    forecast_data %>% 
      align_forecasts() %>% 
      filter(relative_horizon %in% forecast_rel_horizons) -> forecast_data
    
    save(forecast_data, file = paste0(raw_forecast_loc, cur_date, '_all-individual-forecasts.rda'))
  }
}


get_truth_data <- function(truth_target, 
                           truth_data_filename){
  ## Pulls and saves truth data for analysis
  if(truth_target == 'hosp'){
    truth_data <- load_truth(
      truth_source = "HealthData", 
      target_variable = "inc hosp", 
      locations = NULL
    ) 
  } else if(truth_target == 'death'){
    truth_data <- load_truth(
      truth_source = "JHU", 
      target_variable = "inc death", 
      locations = NULL
    )
  } else if(truth_target == 'case'){
    truth_data <- load_truth(
      truth_source = "JHU", 
      target_variable = "inc case", 
      locations = NULL
    )
  } else if(truth_target == 'flu_hosp'){
    truth_data <- load_truth(
      truth_source = "HealthData", 
      target_variable = "inc flu hosp", 
      locations = NULL,
      hub = 'FluSight'
    )
  } else{
    stop("Have not defined metric properly to pull truth data")
  }
  save(truth_data, file = truth_data_filename)
}


get_k_model_combinations <- function(k, models){
  n <- length(models)  
  combn(n, m = k)  %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(k = k,
           combo_num = seq_along(k)) %>% 
    gather(model_num, model, -k, -combo_num) %>% 
    arrange(combo_num) %>% 
    select(k, combo_num, model)
}

get_all_model_combinations <-function(models,
                                      analysis_name){
  # browser()
  ## Gets the model combination lookup table for the analysis
  tibble(k = 1:length(models),
         model_combinations = map(k, get_k_model_combinations, 
                                  models = models)) %>% 
    select(-k) %>% 
    unnest(model_combinations) %>% 
    mutate(model = models[model]) %>% 
    group_by(k, combo_num) %>% 
    filter(k == 1 | !('COVIDhub-4_week_ensemble' %in% model)) %>% 
    filter(k == 1 | !('Flusight-ensemble' %in% model)) %>% 
    ungroup() %>% 
    nest(data = 'model') %>% 
    mutate(combination_num = seq_along(combo_num)) %>% 
    select(combination_num, k, data) %>% 
    unnest(data) -> model_combination_lookup
  
  ## Save lookup table for future reference if needed
  write_csv(model_combination_lookup, 
            file = paste0('raw-data/',analysis_name, 
                          '-model-combination-lookup-table.csv'))
}
