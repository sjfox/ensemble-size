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
