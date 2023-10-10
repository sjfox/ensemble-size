# Ensemble forecast performance by size

## Introduction


## Steps to run analysis

1. Run `R/pull_and_save_case.R`
1. Run `R/pull_and_save_death.R`
1. Run `R/pull_and_save_hosp2.R`
1. Run `R/pull_and_save_fluhosp.R`
1. Download multiyear forecast folders from: https://github.com/FluSightNetwork/cdc-flusight-ensemble/tree/first-papers/model-forecasts/component-models, and save the `component-models/` folder into the `raw-data/` folder
1. Run `R/create-multiyear-lookuptable.R`
1. If running analyses on TACC, need to make sure that all downloaded components are transferred to TACC system. Includes all truth data, lookup tables, and individual component forecasts.
1. Run `tacc_scripts/create-scoring-task-script.R` and `tacc_scripts/multiyear-task-creation.R` to get the task list for the supercomputer.
1. Update the corresponding slurm files for running: `launcher/make-ensembles.slurm` and `launcher/make-multiyear.slurm`
1. Run slurm script, which will call `tacc_scripts/create-and-score-ensembles.R` and `score-multiyear-ensembles.R`
1. Output on tacc is saved to respective folders in the work directory, these can be downloaded and saved locally into the `processed-data/` folder.
1. Run the summarize scripts (`summarize-recent-ensemble-scores.R` and `summarize-multiyear-ensemble-scores.R`)
1. Run `make-overall-forecast-plots.R`
1. Run `make-raw_forecast-plots.R`
1. Run `make-ts-performance-plots.R`
1. Run `make-horizon-performance-plots.R`
1. Run `make-density-rank-plots.R`
1. Run `make-prediction-interval-plots.R`
1. Run `pull-manuscript-stats.R`



## Contact
Contact Spencer Fox (sjfox@uga.edu) with any questions.

