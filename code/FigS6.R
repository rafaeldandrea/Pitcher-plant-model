library(tidyverse)
library(parallel)
library(furrr)

plan(multisession, workers = 120)

dir = '/gpfs/projects/DAndreaGroup/BEF/data/'
folder = '2023-05-29_scenarios_3000_to_3999/'

df = 
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
  expand_grid(seed = 1:50) |> 
  filter(
    production == 'parallel',
    handling_time == 'flat',
    efficiency == 'increase',
    generalism == 'specialists', 
    density_dependence == 'inter',
    n > 40,
    n < 50
  ) |>
  mutate(filename = paste0('scenario-', scenario, '_seed-', seed, '.rds'))

processed_data =
  df |>
  future_pmap_dfr(
    .f = \(scenario, n, seed, filename, ...){
      foo =  
        missing_species =
        readRDS(paste0(dir, folder, filename)) |>
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
  
saveRDS(processed_data, paste0(dir, 'results_figS6.rds'))
