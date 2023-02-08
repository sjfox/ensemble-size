library(tidyverse)
library(here)

if(grepl('spencerfox', Sys.info()['user'])){
  raw_forecast_loc <- here(file.path('raw-data'))
} else if (grepl('frontera', Sys.info()['nodename'])){
  raw_forecast_loc <- here(file.path('raw-data'))
}

# analysis_names <- c('hosp1', 'hosp2', 'death', 'case', 'flu_hosp')
analysis_names <- 'flu_hosp'
sink(here('launcher/ensemble-creation-tasks.txt'))
for(analysis_name in analysis_names){
  model_combos <- read_csv(here(file.path(raw_forecast_loc,
                                                paste0(analysis_name, 
                                                '-model-combination-lookup-table.csv'))),
                           progress = FALSE) %>% 
    pull(combination_num) %>% unique()

  for(model_num in model_combos){
      startCmd <- "R CMD BATCH --no-restore --no-save '--args"
      fileCmd <- paste0(' analysis_name="', analysis_name, 
                        '" ensemble_num="', model_num, '"')
      endCmd <- "' ../tacc-scripts/create-and-score-ensembles.R"
      full_cmd <- paste0(startCmd, fileCmd, endCmd)
      # print(full_cmd)
      cat(full_cmd)               # add command
      cat('\n')              # add new line  
  }  
}
sink()



