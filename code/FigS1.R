library(tidyverse)
library(parallel)
library(furrr)

plan(multisession, workers = 120)

Diversity = 
  \(dat){
    species_abundances = 
      dat |> 
      filter(grepl('N', species)) |>
      filter(value > 0)
    
    abundance_threshold = 
      species_abundances |>
      pull(value) |>
      log() |>
      quantile(.75)
    
    Shannon = 
      \(N){
        if(sum(N > 0) == 0) return(0)
        n = N[N > 0]
        n = n[log(n) > abundance_threshold]
        p = n / sum(n)
        shannon = -sum(p * log(p))
        return(shannon)
      }
    
    Richness = 
      \(N){
        n = N[N > 0]
        sum(log(n) > abundance_threshold)
      }
    
    result = 
      species_abundances |>
      group_by(time) |>
      summarize(
        richness = Richness(value),
        shannon = Shannon(value), 
        .groups = 'drop'
      )
    
    return(
      dat |>
        select(scenario:seed) |>
        unique() |>
        mutate(
          max_richness = max(result$richness),
          max_shannon = max(result$shannon)
        )
    )
  }

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
  filter(density_dependence == 'none') |>
  mutate(filename = paste0('scenario-', scenario, '_seed-', seed, '.rds'))

dir = '/gpfs/projects/DAndreaGroup/BEF/data/'
folders = 
  c(
    '2023-05-29_scenarios_0001_to_0999/',
    '2023-05-29_scenarios_1000_to_1999/',
    '2023-05-29_scenarios_2000_to_2999/',
    '2023-05-29_scenarios_3000_to_3999/'
  )

processed_data =
  folders |>
  map_dfr(
    .f = \(folder){
      list.files(paste0(dir, folder)) |>
        intersect(df$filename) |>
        future_map_dfr(
          .f = \(file){
            readRDS(paste0(dir, folder, file)) |>
              Diversity()
          }
        )
    }
  )

saveRDS(processed_data, paste0(dir, 'results_figS1.rds'))
