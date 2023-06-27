# Pitcher-plant-model
Files for pitcher plant BEF project

Code files are used as follows.

**simulation_script.R**
Defines all model scenarios. Calls _project_functions.R_. Creates folder to save data files. Chunks model scenarios to run in parallel in HPC cluster. Runs all simulations (3672 scenarios with 50 replicates each). Data folders are named _[DATE OF SIMULATION]_scenarios_[MIN SCENARIO]_to_[MAX SCENARIO]_, broken down into four groups: a. scenarios 1 to 999; b. scenarios 1000 to 1999; c. scenarios 2000 to 2999; d. scenarios 3000 to 3999.

**project_functions.R**
Defines functions needed to simulate model scenarios.
•	_pitcher_: calculates increments to population sizes, resource concentrations, and ammonia produced by each species. 
•	_parameters_: defines parameter values used by _pitcher_ based on model scenarios in the call to _simulation_script.R_.
•	_simulation_: receives model scenarios, calls parameters, then runs ode from package deSolve using pitcher. Saves one file for each scenario x replicate as _scenario-[SCENARIO ID]_seed-[REPLICATE ID].rds_. Each data file contains dynamic data for all resources and seeded species, as well as ammonia produced by each species at each time increment. 

**read_raw_data_save_results.R**
For each data folder, reads every data file, extracts total function by species at the end of the simulation. Saves results into three files, organized by niche scenario: _[DATE OF SIMULATION]_results_generalists.rds_, _[DATE OF SIMULATION]_results_specialists.rds_, _[DATE OF SIMULATION]_results_gradient.rds_.

**figS1.R** 
Defines realized richness as the maximum number of species across one simulation replicate (see details in main text). For each data folder, reads every data file, extracts richness, saves resulting data as _[DATE OF SIMULATION]_results_figS1.rds_.

**figS5.R**
Reads every data file within the Specialists niche scenario with Interspecific co-regulation, Nested by-production architecture, Flat handling time, and Increase resource quality. For each replicate, records species ID of species present in the pool but not seeded in the pitcher plant (missing species) and total function. Saves resulting data as _[DATE OF SIMULATION]_results_figS5.rds_.

**make_figures.R**
Reads all needed data files from GitHub (see **Metadata**). Creates plots for all data figures in the paper. Those include main text Figs. 2-5 and supplementary Figs. S1-S5. Creates Table S3 (which is also the source for the data file _2023-06-16_BEF_shape_table.csv_ used in Fig. 5).

**Note: Raw data files are stored in Zenodo, DOI XXX.**
