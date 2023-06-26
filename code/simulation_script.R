library(tidyverse) ## for tidy data wrangling and piping
library(deSolve) ## for ODE integration
library(parallel) ## for parallel computing
library(furrr) ## for parallel computing
library(gtools) ## for sampling

## fixed parameters
fixed_parameters = 
  list(
    num_resources = 50,
    num_species = 50,
    p0 = .7,
    beta_max = 10,
    lambda_min = 1,
    lambda_max = 10,
    c_min = .8,
    c_max = 8,
    alpha0 = 1e-17,
    mu = .01,
    rho0 = 2,
    tau = 1,
    logh_min = log(1),
    logh_max = log(8),
    qmin = .05,
    qmax = .1,
    num_replicates = 50
  ) 

list2env(fixed_parameters, envir = .GlobalEnv)

## load custom functions 
source('project_functions.R')

## set number of workers for parallel processing
plan(cluster, workers = 120)

## read batch array index (separates scenario simulations into chunks run by different nodes)
array_id = 
  'SLURM_ARRAY_TASK_ID' |> 
  Sys.getenv() |> 
  as.numeric()

## parameter scenarios
scenarios = 
   expand_grid(
      n = c(seq(1, 17, 2), seq(19, 49, 5), 50),
      ammonia_capacity = 'Decrease',
      coregulation = c('Absent', 'Neutral', 'Intraspecific', 'Interspecific'),
      byproduction = c('Serial', 'Nested', 'None'),
      handling_time = c('Decrease', 'Flat', 'Increase'),
      niches = c('Generalists', 'Gradient', 'Specialists'),
      quality = c('Flat', 'Increase')
   ) |>
   rowid_to_column(var = 'scenario') |>
   expand_grid(seed = 1:num_replicates)

## chunk scenarios based on array_id
chunks = 
  tibble(
    minscen = seq(1, max(scenarios$scenario), by = round(1e3 / num_replicates)),
    maxscen = minscen + round(1e3 / num_replicates) - 1,
    datadir = 1e3 * (minscen %/% 1e3)
  )

minscen = chunks$minscen[array_id]
maxscen = chunks$maxscen[array_id]
datadir = chunks$datadir[array_id]

## directory to save files
dir_path = 
  paste0(
    '/gpfs/projects/DAndreaGroup/BEF/data/', 
    Sys.Date(),
    '_scenarios_',
    datadir,
    '_to_',
    datadir + 999
  )

dir.create(dir_path, showWarnings = FALSE)

## run simulation and save results to dir_path
results =
   scenarios |>
   filter(scenario %in% minscen:maxscen) |>
   future_pmap_dfr(
      simulation,
      t_max = 1000,
      full.data = TRUE,
      save.file = TRUE,
      path = dir_path,
      .options = furrr_options(seed = NULL)
   )
