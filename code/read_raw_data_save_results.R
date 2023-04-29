library(tidyverse)
library(parallel)
library(furrr)

plan(multisession, workers = 30)

data_folder = '2023-03-30/'

dir_path = 
   paste0('g:/My Drive/Stony Brook University/Conferences/NSF Montreal Workshop/Function group/Data/', data_folder)

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

saveRDS(results, paste0(dir_path, 'results.rds'))
