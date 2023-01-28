library(tidyverse)
library(here)

analysis_names <- c('hosp1', 'hosp2', 'death', 'case')

sink(here('launcher/ensemble-creation-tasks.txt'))
for(analysis_name in analysis_names){
  forecasts <- list.files(here(paste0('raw-data/', analysis_name)), full.names = F)
  forecast_dates <- str_replace(forecasts, '_all-individual-forecasts.rda', '')
  
  metric <- case_when(analysis_name == 'death' ~ 'death',
                      analysis_name == 'case' ~ 'case',
                      T ~ 'hosp')
    
  for(forecast_date in forecast_dates){
      startCmd <- "R CMD BATCH --no-restore --no-save '--args"
      fileCmd <- paste0(' analysis_name="', analysis_name, 
                        '" forecast_date="', forecast_date, 
                        '" metric="', metric, '"')
      endCmd <- "' ../tacc_scripts/create-and-score-ensembles.R"
      full_cmd <- paste0(startCmd, fileCmd, endCmd)
      # print(full_cmd)
      cat(full_cmd)               # add command
      cat('\n')              # add new line  
  }  
}
sink()



