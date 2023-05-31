library(tidyverse)
library(parallel)
library(furrr)

plan(multisession, workers = 30)

data_folders = 
  c(
    '2023-05-29_scenarios_0_to_999',
    '2023-05-29_scenarios_1000_to_1999',
    '2023-05-29_scenarios_2000_to_2999',
    '2023-05-29_scenarios_3000_to_3999'
  )

for(folder in data_folders){
  dir_path = 
    paste0(
      '/gpfs/projects/DAndreaGroup/BEF/data/', 
      folder
    )
  
  filenames = list.files(dir_path, pattern = '*.rds', full.names = TRUE)
  
  results = 
    filenames |>
    future_map_dfr(
      .f = \(file){
        foo = try(readRDS(file), silent = TRUE)
        if(!inherits(foo, "try-error")){
          bar = 
            foo |>
            filter(grepl('F', species)) |> 
            slice_max(time)
          return(bar)
        } else return()
      } 
    ) |>
    mutate(
      density_dependence = 
        fct_recode(
          density_dependence, 
          !!!c(
            Interspecific = 'inter', 
            Intraspecific = 'intra', 
            Neutral = 'neutral', 
            None = 'none'
          )
        ),
      generalism = 
        fct_recode(
          generalism,
          !!!c(
            Generalists = 'random',
            Specialists = 'specialists',
            Gradient = 'tradeoff'
          )
        )
    )
  
  saveRDS(results, paste0('results_', folder,'.rds'))
  
}

setwd('/gpfs/projects/DAndreaGroup/BEF/data/')
results = NULL
for(folder in data_folders){
  results =
    results |>
    bind_rows(
      readRDS(paste0('results_', folder, '.rds'))
    )
}

results_gradient = 
  results |> 
  filter(generalism == 'tradeoff')

results_specialists = 
  results |> 
  filter(generalism == 'specialists')

results_generalists = 
  results |> 
  filter(generalism == 'random')

saveRDS(results, 'results_2023-05-29.rds')
saveRDS(results_generalists, 'results_generalists.rds')
saveRDS(results_specialists, 'results_specialists.rds')
saveRDS(results_gradient, 'results_gradient.rds')