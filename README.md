# Cheating, monitoring, and sanctioning in an agent-based model of community resource management

This repository contains the models, data, and analysis for the article "Cheating, monitoring, and sanctioning in an agent-based model of community resource management" 

## `simulations_and_model_code/`
Folder containing model code and code used to run the sensitivity analysis simulations. Any single simulation runs were set up and run directly in NetLogo and exported via the NetLogo GUI (using NetLogo's `export plots` function) to a csv file. 

## `analysis/` 
Folder containing data from simulation runs, code to clean the data, figures, and code to replicate the figures. 

Important files: 
- `publication_figures_1.3.2021.Rmd`: A file replicating all figures in the article using the data given in allData_IS/
- `dataCleaningFunctions.R`: File with functions used for data cleaning (and when creating select figures) 
- `dataCleaningFunctions.R`: script used to convert data files from running NetLogo simulations into usable data 
- `process_m&s_data.R`: script used to clean M&S model data
- `process_c&e_data.R`: script used to clean C&E model data

### `allData_IS`
Folder containing the raw and cleaned data from the simulation runs. 

Improtant files: 
- `endogInst_model_run.csv`: data from single run of the Endogenous Institution model under base parameters
- `one-third-bmax.csv`: data from single run of the Endogenous Institution model under base parameters but with `tolerance-threshold = 1/3 bmax`
- `log1000.csv`: data from single run of the Cheating and Enforcement model under base parameters but with `initial-loggers = 1000`
- `MS_model_data_clean.RData`: cleaned data from Monitoring and Sanctioning model sensitivity analysis 
- `CE_model_data_clean.RData`: cleaned data from Cheating and Enforcement model sensitivity analysis 
- vallinoData: raw data from C&E sensitivity analysis
   - `process_c&e_data.R`: file that processes raw data into the CE_model_data_clean.RData clean data format 
   - one .RData file for each parameter that was varied. When loaded contains a single variable called dataOut
   - one csv file for a dataframe containing summary statistics for each of the simulation runs
- M&S_model_data: raw data from M&S sensitivity analysis
   - `process_m&s_data.R`: file that processes raw data into the CE_model_data_clean.RData clean data format 
   - 2 .RData file for each parameter that was varied. When loaded contains a single variable called dataOut. The two seperate sets of data were run using the same parameters and are interchangable 
   - one csv file for a dataframe containing summary statistics for each of the simulation runs



## Todo
Add metadata 
