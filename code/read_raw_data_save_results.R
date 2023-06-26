library(tidyverse)
library(parallel)
library(furrr)

plan(multisession, workers = 30)

dir_date = '2023-06-16'
dir_path = 
  paste0(
    '/gpfs/projects/DAndreaGroup/BEF/data/', 
    dir_date
  )

data_folders = 
  paste0(
    dir_path,
    c(
      '_scenarios_0_to_999',
      '_scenarios_1000_to_1999',
      '_scenarios_2000_to_2999',
      '_scenarios_3000_to_3999'
    )
  )

for(folder in data_folders){
  
  filenames = list.files(folder, pattern = '*.rds', full.names = TRUE)
  
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
    )
  
  saveRDS(results, paste0(folder,'_results.rds'))
  
}

results = NULL
for(folder in data_folders){
  results =
    results |>
    bind_rows(
      readRDS(paste0(folder,'_results.rds'))
    )
}

saveRDS(results, paste0(dir_path, '_results.rds'))

results_gradient = 
  results |> 
  filter(niches == 'Gradient')

results_specialists = 
  results |> 
  filter(niches == 'Specialists')

results_generalists = 
  results |> 
  filter(niches == 'Generalists')


saveRDS(results_generalists, paste0(dir_path, '_results_generalists.rds'))
saveRDS(results_specialists, paste0(dir_path, '_results_specialists.rds'))
saveRDS(results_gradient,    paste0(dir_path, '_results_gradient.rds'))
