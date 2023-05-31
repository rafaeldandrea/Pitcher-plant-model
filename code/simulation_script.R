library(tidyverse) ## for tidy data wrangling and piping
library(deSolve) ## for ODE integration
library(parallel) ## for parallel computing
library(furrr) ## for parallel computing
library(gtools) ## for sampling

## fixed parameters
totalspecies = 50

fixed_parameters = 
   list(
      p0 = .7,
      beta0 = 10,
      lambda_min = 1,
      lambda_max = 10,
      gamma_min = .8,
      gamma_max = 8,
      alpha0 = 1e-17,
      chain_length = totalspecies,
      mu0 = .01,
      rho0 = 2,
      hmin = log(1),
      hmax = log(8)   
   ) 

list2env(fixed_parameters, envir = .GlobalEnv)

## load custom functions 
source('project_functions.R')

## set number of workers for parallel processing
plan(multisession, workers = 120) 

dir_path = 
   paste0(
      '/gpfs/home/rdrocha/BEF/data/', 
      Sys.Date()
   )
dir.create(dir_path, showWarnings = FALSE)

scenarios = 
   expand_grid(
      n = c(seq(1, 17, 2), seq(19, 49, 5), 50),
      sampling = 'random',
      resource_supply = 'uniform',
      ammonia = 'decrease',
      density_dependence = c('none', 'neutral', 'intra', 'inter'),
      production = c('serial', 'parallel', 'off'),
      recalcitrance = TRUE,
      handling_time = c('decrease', 'flat', 'increase'),
      generalism = c('random', 'tradeoff', 'specialists'),
      uptake_factor = 1,
      efficiency = c('flat', 'increase'),
      maxtime = 1000
   ) |>
   rowid_to_column(var = 'scenario') |>
   expand_grid(seed = 1:50)


results =
   scenarios |>
   future_pmap_dfr(
      simulation,
      full.data = TRUE,
      save.file = TRUE,
      path = dir_path,
      .options = furrr_options(seed = NULL)
   )
