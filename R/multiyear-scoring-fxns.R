### Scoring functions

read_entry <- function (file) {
  ## Copied from FluSight package
  entry <- read.csv(file, colClasses = "character", stringsAsFactors = FALSE)
  names(entry) <- tolower(names(entry))
  entry <- entry %>% mutate(value = as.numeric(value), 
                            bin_start_incl = trimws(replace(bin_start_incl, 
                                                            !is.na(bin_start_incl) & bin_start_incl != "none", 
                                                            format(round(as.numeric(bin_start_incl[!is.na(bin_start_incl) & bin_start_incl != "none"]), 1), 
                                                                   nsmall = 1))), 
                            bin_end_notincl = trimws(replace(bin_end_notincl, 
                                                             !is.na(bin_end_notincl) & bin_end_notincl != "none", 
                                                             format(round(as.numeric(bin_end_notincl[!is.na(bin_end_notincl) & bin_end_notincl != "none"]), 1), 
                                                                    nsmall = 1))))
  forecast_week <- as.numeric(gsub("EW", "", regmatches(file, 
                                                        regexpr("(?:EW)[0-9]{2}", file))))
  if (length(forecast_week > 0)) 
    entry <- dplyr::mutate(entry, forecast_week = forecast_week)
  entry %>% dplyr::arrange(type, location, target) %>% 
    dplyr::select(location, target, type, 
                  unit, bin_start_incl, bin_end_notincl, 
                  value, everything())
}

stack_forecasts <- function(files) {
  require(dplyr)
  
  nfiles <- length(files)
  files %>% 
    map(read_entry) %>% 
    bind_rows() %>% 
    filter(type!='Point') %>% 
    mutate(weighted_val = 1/nfiles*value) %>% 
    select(-value) %>% 
    group_by(forecast_week, location, target, type, unit, bin_start_incl, bin_end_notincl) %>% 
    summarize(value = sum(weighted_val), .groups = 'drop') 
}

score_multi_entry <- function(entry, truth){
  names(entry) <- tolower(names(entry))
  names(truth) <- tolower(names(truth))
  if (!("forecast_week" %in% names(entry))) 
    stop("Column forecast_week needed in entry - \n         use read_entry() with your submission CSV")
  seasonal <- entry %>% 
    filter(type == "Bin", 
           target %in% c("Season onset", 
                         "Season peak week", 
                         "Season peak percentage", 
                         "Season peak rate")) %>% 
    right_join(truth, by = c("location", "target", "bin_start_incl")) %>% 
    filter(target %in% c("Season onset", "Season peak week", 
                         "Season peak percentage", "Season peak rate")) %>% 
    select(-forecast_week.y) %>% 
    rename(forecast_week = forecast_week.x)
  weekly <- entry %>% 
    filter(type == "Bin", target %in% c("1 wk ahead", 
                                        "2 wk ahead", 
                                        "3 wk ahead", 
                                        "4 wk ahead")) %>% 
    right_join(truth, by = c("location", "target", 
                             "bin_start_incl", "forecast_week")) %>% 
    filter(target %in% c("1 wk ahead", 
                         "2 wk ahead", 
                         "3 wk ahead", 
                         "4 wk ahead"))
  # browser()
  scores <- bind_rows(seasonal, 
                      weekly) %>% 
    group_by(location, 
             target, 
             forecast_week) %>% 
    summarize(score = log(sum(value)), .groups = 'drop') %>% 
    mutate(score = ifelse(score < -10 | is.na(score), -10, score))
  return(scores)
}

get_scored_seasons <- function(task_models, 
                               raw_forecast_loc,
                               save_loc,
                               model_name,
                               seasons = c("2010/2011", 
                                           "2011/2012", 
                                           "2012/2013", 
                                           "2013/2014", 
                                           "2014/2015", 
                                           "2015/2016", 
                                           "2016/2017"),
                               save_scores = T){
  scored_seasons <- vector('list', length(seasons))
  for(i in 1:length(seasons)){
    loso_season =  seasons[i]
    
    ## identify the "EWXX-YYYY" combos for files given season
    first_year <- substr(loso_season, 0, 4)
    first_year_season_weeks <- if(first_year==2014) {43:53} else {43:52}
    week_names <- c(
      paste0("EW", first_year_season_weeks, "-", first_year),
      paste0("EW", formatC(1:18, width=2, flag=0), "-", as.numeric(first_year)+1)
    )
    
    ## Manage truth data for scoring
    if(!dir.exists(here('raw-data/multiyear-truth'))){
      dir.create(here('raw-data/multiyear-truth'))
    }
    truth_file <- here(paste0('raw-data/multiyear-truth/', first_year, '-ili.rda'))
    if(file.exists(truth_file)){
      load(truth_file)
    }else{
      truth_df <- FluSight::create_truth(year = as.numeric(first_year))
      save(truth_df, file = truth_file)
    }
    
    ensemble_forecasts <- vector('list', length = length(week_names))
    for(k in 1:length(week_names)){
      files_to_stack <- here(paste0(raw_forecast_loc, 
                                    "/component-models/", 
                                    task_models$model, '/',
                                    week_names[k], "-", 
                                    task_models$model, ".csv"))
      stacked_entry <- stack_forecasts(files_to_stack)
      ensemble_forecasts[[k]] <- stacked_entry
    }
    scored_seasons[[i]] <- score_multi_entry(ensemble_forecasts %>% 
                                               bind_rows(), 
                                             truth_df) 
  }
  if(save_scores){
    if(!dir.exists(save_loc)){
      dir.create(save_loc)
    }
    scored_seasons %>% 
      bind_rows(.id = 'id') %>% 
      mutate(season = seasons[as.numeric(id)]) %>% 
      select(-id) %>% 
      mutate(model = model_name) %>%  
      write_csv(paste0(save_loc, '/', model_name,'_scores.csv'))  
  } else{
    scored_seasons %>% 
      bind_rows(.id = 'id') %>% 
      mutate(season = seasons[as.numeric(id)]) %>% 
      select(-id) %>% 
      mutate(model = model_name)
  }
}
