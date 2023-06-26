library(tidyverse)
library(parallel)
library(furrr)

plan(multisession, workers = 120)

date = '2023-06-16'
dir = '/gpfs/projects/DAndreaGroup/BEF/data/'
folder = '_scenarios_3000_to_3999/'

df = 
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
  expand_grid(seed = 1:50) |> 
  filter(
    niches == 'Specialists', 
    coregulation == 'Interspecific',
    byproduction == 'Nested',
    handling_time == 'Flat',
    quality == 'Increase',
    n > 40,
    n < 50
  ) |>
  mutate(filename = paste0('scenario-', scenario, '_seed-', seed, '.rds'))

processed_data =
  df |>
  future_pmap_dfr(
    .f = \(scenario, n, seed, filename, ...){
      foo =  
        readRDS(paste0(dir, date, folder, filename)) |>
        slice_max(time)  
      
      dtf = 
        foo |>
        filter(grepl('N', species)) |>
        pull(species) |>
        unique() |>
        setdiff(x = paste0('N', 1:50)) |>
        str_split('N', simplify = TRUE) |>
        as.data.frame() |>
        as_tibble() |>
        mutate(
          scenario = scenario,
          n = n,
          seed = seed
        ) |>
        mutate(
          missing_species = as.numeric(V2),
          func = 
            foo |> 
            filter(species == 'function') |> 
            pull(value)
        ) |>
        select(scenario, seed, missing_species, func)
      
      return(dtf)
    }
  ) |>
  inner_join(df)
  
saveRDS(processed_data, paste0(dir, date, '_results_figS5.rds'))
