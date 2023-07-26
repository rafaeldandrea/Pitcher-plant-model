# Pitcher-plant-model
Files for pitcher plant BEF project

Code files are used as follows.

**simulation_script.R**

Defines all model scenarios. Calls _project_functions.R_. Creates folder to save data files. Chunks model scenarios to run in parallel in HPC cluster. Runs all simulations (3672 scenarios with 50 replicates each). Data folders are named _[DATE OF SIMULATION]_scenarios_[MIN SCENARIO]_to_[MAX SCENARIO]_, broken down into four groups: a. scenarios 1 to 999; b. scenarios 1000 to 1999; c. scenarios 2000 to 2999; d. scenarios 3000 to 3999.

<br>

**project_functions.R**

Defines functions needed to simulate model scenarios.
•	_pitcher_: calculates increments to population sizes, resource concentrations, and ammonia produced by each species. 
•	_parameters_: defines parameter values used by _pitcher_ based on model scenarios in the call to _simulation_script.R_.
•	_simulation_: receives model scenarios, calls parameters, then runs ode from package deSolve using pitcher. Saves one file for each scenario x replicate as _scenario-[SCENARIO ID]_seed-[REPLICATE ID].rds_. Each data file contains dynamic data for all resources and seeded species, as well as ammonia produced by each species at each time increment. 

<br>

**read_raw_data_save_results.R**

For each data folder, reads every data file, extracts total function by species at the end of the simulation. Saves results into three files, organized by niche scenario: _[DATE OF SIMULATION]_results_generalists.rds_, _[DATE OF SIMULATION]_results_specialists.rds_, _[DATE OF SIMULATION]_results_gradient.rds_.

<br>

**figS1.R** 

Defines realized richness as the maximum number of species across one simulation replicate (see details in main text). For each data folder, reads every data file, extracts richness, saves resulting data as _[DATE OF SIMULATION]_results_figS1.rds_.

<br>

**figS5.R**

Reads every data file within the Specialists niche scenario with Interspecific co-regulation, Nested by-production architecture, Flat handling time, and Increase resource quality. For each replicate, records species ID of species present in the pool but not seeded in the pitcher plant (missing species) and total function. Saves resulting data as _[DATE OF SIMULATION]_results_figS5.rds_.

<br>

**make_figures.R**

Reads all needed data files from GitHub (see **Metadata**). Creates plots for all data figures in the paper. Those include main text Figs. 2-5 and supplementary Figs. S1-S5. Creates Table S3 (which is also the source for the data file _2023-06-16_BEF_shape_table.csv_ used in Fig. 5).

<br>

**Note: Raw data files are stored in Zenodo, DOI XXX.**

<br>

R Session Info

R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 22621)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
[4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

time zone: America/New_York
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] randomForestSRC_3.2.2 randomForest_4.7-1.1  scales_1.2.1          ggh4x_0.2.4           lubridate_1.9.2      
 [6] forcats_1.0.0         stringr_1.5.0         dplyr_1.1.2           purrr_1.0.1           readr_2.1.4          
[11] tidyr_1.3.0           tibble_3.2.1          ggplot2_3.4.2         tidyverse_2.0.0      

loaded via a namespace (and not attached):
 [1] DiagrammeR_1.0.10  utf8_1.2.3         generics_0.1.3     stringi_1.7.12     hms_1.1.3          digest_0.6.31     
 [7] magrittr_2.0.3     grid_4.3.1         timechange_0.2.0   RColorBrewer_1.1-3 fastmap_1.1.1      jsonlite_1.8.5    
[13] fansi_1.0.4        cli_3.6.1          rlang_1.1.1        visNetwork_2.1.2   cowplot_1.1.1      munsell_0.5.0     
[19] withr_2.5.0        tools_4.3.1        parallel_4.3.1     tzdb_0.4.0         colorspace_2.1-0   vctrs_0.6.3       
[25] R6_2.5.1           lifecycle_1.0.3    htmlwidgets_1.6.2  pkgconfig_2.0.3    pillar_1.9.0       gtable_0.3.3      
[31] glue_1.6.2         tidyselect_1.2.0   data.tree_1.0.0    rstudioapi_0.14    farver_2.1.1       htmltools_0.5.5   
[37] labeling_0.4.2     compiler_4.3.1 
