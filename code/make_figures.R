library(tidyverse) ## for tidy data wrangling and piping
library(ggh4x) ## for function facet_grid2()

theme_set(theme_bw())
theme_update(
   panel.grid = element_blank(),
   strip.background = element_blank(),
   aspect.ratio = 1
)

results_generalists = 
   'https://github.com/rafaeldandrea/Pitcher-plant-model/blob/main/data/results_generalists.rds?raw=true' |>
   url() |>
   readRDS()

results_specialists =
   'https://github.com/rafaeldandrea/Pitcher-plant-model/blob/main/data/results_specialists.rds?raw=true' |>
   url() |>
   readRDS()

results_gradient =
   'https://github.com/rafaeldandrea/Pitcher-plant-model/blob/main/data/results_gradient.rds?raw=true' |>
   url() |>
   readRDS()

results =
   results_generalists |>
   bind_rows(results_specialists) |>
   bind_rows(results_gradient)

missing_species = 
   'https://github.com/rafaeldandrea/Pitcher-plant-model/blob/main/data/missing_species.rds?raw=true' |>
   url() |>
   readRDS()

## Total function across consumer species
total_function =
   results |>
   group_by(across(scenario:time)) |>
   summarize(
      func = sum(value),
      .groups = 'drop'
   ) |>
   mutate(
      generalism = 
         fct_recode(
            generalism,
            !!!c(
               Generalists = 'random',
               Specialists = 'specialists',
               Gradient = 'tradeoff'
            )
         ),
      density_dependence =
         fct_recode(
            density_dependence,
            !!!c(
               Absent = 'none',
               Intraspecific = 'intra',
               Interspecific = 'inter',
               Neutral = 'neutral'
            )
         )
   ) |>
   mutate(
      density_dependence = 
         factor(
            density_dependence, 
            levels = 
               c(
                  'Absent', 
                  'Neutral', 
                  'Intraspecific', 
                  'Interspecific'
               )
            )
   )

## Average total function across scenario replicates
total_function_summary =
   total_function |>
   group_by(across(scenario:efficiency)) |>
   summarize(
      func = mean(func),
      .groups = 'drop'
   )

## Fig2 -------------------------------------------------------------------
## Plot BEF for scenarios with increasing resource quality, flat resource recalcitrance, and nested by-production
## include data points for all replicates
fig2 = 
   total_function |>
   filter(
      efficiency == 'increase',
      handling_time == 'flat',
      production == 'parallel'
   ) |>
   ggplot(aes(n, func)) +
   geom_point(color = 'grey90') +
   geom_line(
      aes(n, func), 
      data = 
         total_function_summary |>
         filter(
            efficiency == 'increase',
            handling_time == 'flat',
            production == 'parallel'
         )
   ) +
   theme(legend.position = 'none') +
   ggh4x::facet_grid2(density_dependence ~ generalism, scales = 'free_y', independent = 'y') +
   labs(x = 'Species richness', y = 'Total ammonia production')



## Fig3 -------------------------------------------------------------------
## Plot BEF for scenarios with flat resource recalcitrance
## include data points for all replicates
## Note: Fig3: .ht = 'flat';  FigS2: .ht = 'increase';  FigS3: .ht = 'decrease'
for(.ht in c('flat', 'decrease', 'increase')){
   plot = 
      total_function_summary |>
      filter(handling_time == .ht) |>
      mutate(
         efficiency = 
            fct_recode(
               efficiency,
               !!!c(
                  Neutral = 'flat',
                  Gradient = 'increase'
               )
            ),
         production =
            fct_recode(
               production,
               !!!c(
                  None = 'off',
                  Nested = 'parallel',
                  Serial = 'serial'
               )
            )
      ) |>
      ggplot(aes(n, func, linetype = efficiency, color = production)) +
      geom_line() +
      ggh4x::facet_grid2(density_dependence ~ generalism, scales = 'free_y', independent = 'y') +
      labs(
         x = 'Species richness', 
         y = 'Total ammonia production',
         color = 'By-production\narchitecture',
         linetype = 'Resource\nquality'
      )
   
   assign(paste0('fig3_', .ht), plot)
}



## Fig4? -------------------------------------------------------------------
## Calculate complementarity and selection effects
## Reference: Loreau & Hector 2001 (Nature)
Loreau = 
   results |>
   inner_join(
      results |>
         filter(n == 1) |>
         rename(monoculture = value) |>
         select(-c(scenario, n, seed))
   ) |>
   mutate(
      N = n,
      Mi = monoculture,
      YOi = value,
      RYEi = 1 / N,
      YEi = RYEi * Mi,
      RYOi = YOi / Mi,
      DRYi = RYOi - RYEi
   )

## aggregate the above across species
Hector = 
   Loreau |>
   group_by(across(scenario:time)) |>
   summarize(
      N = unique(N),
      DY = sum(YOi) - sum(YEi),
      Mbar = mean(Mi),
      DRYbar = mean(DRYi),
      compl = N * DRYbar * Mbar,
      selec = N * mean((DRYi - DRYbar) * (Mi - Mbar)),
      compl_scaled = compl / abs(DY),
      selec_scaled = selec / abs(DY),
      .groups = 'drop'
   )

## calculate the mean of the above across scenario replicates
comp_sel = 
   Hector |>
   filter(n > 1) |>
   group_by(across(n:efficiency)) |>
   summarize(
      compl.m = mean(compl),
      compl.s = sd(compl),
      selec.m = mean(selec),
      selec.s = sd(selec),
      .groups = 'drop'
   ) |>
   mutate(
      generalism = 
         fct_recode(
            generalism,
            !!!c(
               Generalists = 'random',
               Specialists = 'specialists',
               Gradient = 'tradeoff'
            )
         ),
      density_dependence =
         fct_recode(
            density_dependence,
            !!!c(
               Absent = 'none',
               Intraspecific = 'intra',
               Interspecific = 'inter',
               Neutral = 'neutral'
            )
         )
   ) |>
   mutate(
      density_dependence = 
         factor(
            density_dependence, 
            levels = 
               c(
                  'Absent', 
                  'Neutral', 
                  'Intraspecific', 
                  'Interspecific'
               )
         )
   )

comp_sel_avg = 
   comp_sel |>
   group_by(across(sampling:efficiency)) |>
   summarize(
      compl.m = mean(compl.m),
      compl.s = sd(compl.m),
      selec.m = mean(selec.m),
      selec.s = sd(selec.m),
      .groups = 'drop'
   )

## Compare scenarios based on complementarity and selection effects 
fig5 = 
   comp_sel_avg |>
   filter(
      handling_time == 'flat',
      efficiency == 'increase',
      production == 'parallel'
   ) |>
   rename(Complementarity = compl.m, Selection = selec.m) |>
   filter(
      handling_time == 'flat',
      efficiency == 'increase',
      production == 'parallel'
   ) |>
   ggplot(
      aes(
         Complementarity, 
         Selection, 
         fill = generalism,
         shape = density_dependence
      )
   ) +
   geom_vline(xintercept = 0, color = 'grey') +
   geom_hline(yintercept = 0, color = 'grey') +
   geom_point(size = 4, color = 'black') +
   labs(
      x = 'Complementarity', 
      y = 'Selection', 
      fill = 'Niche\nscenario', 
      shape = 'Co-regulation\nscenario'
   ) +
   scale_shape_manual(values=c(22, 21, 24, 25)) +
   guides(fill = guide_legend(override.aes = list(color = c('#F8766D','#00BA38','#619CFF'))))

figS4 = 
   comp_sel_avg |>
   filter(
      handling_time == 'increase'
   ) |>
   mutate(
      efficiency = 
         fct_recode(
            efficiency,
            !!!c(
               Neutral = 'flat',
               Gradient = 'increase'
            )
         ),
      production =
         fct_recode(
            production,
            !!!c(
               None = 'off',
               Nested = 'parallel',
               Serial = 'serial'
            )
         )
   ) |>
   ggplot(aes(compl.m, selec.m, fill = generalism, shape = density_dependence)) +
   geom_vline(xintercept = 0, color = 'grey') +
   geom_hline(yintercept = 0, color = 'grey') +
   geom_point() +
   ggh4x::facet_grid2(efficiency ~ production, scales = 'free_y', independent = 'y') +
   labs(
      x = 'Complementarity', 
      y = 'Selection', 
      fill = 'Niche\nscenario', 
      shape = 'Co-regulation\nscenario'
   ) +
   scale_shape_manual(values=c(22, 21, 24, 25)) +
   guides(fill = guide_legend(override.aes = list(color = c('#F8766D','#00BA38','#619CFF'))))



## Fig4? -------------------------------------------------------------------
## Complementarity and Selection by richness
fig4_alternate = #not used
   comp_sel |>
   filter(
      efficiency == 'increase', 
      handling_time == 'flat',
      production == 'parallel'
   ) |>
   rename(Complementarity = compl.m, Selection = selec.m) |>
   pivot_longer(c(Complementarity, Selection), names_to = 'effect') |>
   ggplot(aes(n, value, color = generalism, linetype = density_dependence)) +
   geom_line() +
   facet_grid(~effect) +
   labs(
      x = 'Species richness',
      y = 'Value',
      fill = 'Niche\nscenario',
      linetype = 'Co-regulation\nscenario'
   )

## Complementarity and Selection by richness
figS5 = 
   comp_sel|>
   filter(
      handling_time == 'flat',
      efficiency == 'increase',
      production == 'parallel'
   ) |>
   ggplot() +
   # geom_errorbar(aes(x = n, ymin = selec.m - selec.s, ymax = selec.m + selec.s)) +
   geom_line(aes(n, compl.m), color = 'blue') +
   geom_line(aes(n, selec.m), color = 'red') +
   geom_line(aes(n, compl.m + selec.m), color = 'green') +
   ggh4x::facet_grid2(density_dependence ~ generalism, scales = 'free_y', independent = 'y') +
   labs(
      x = 'Species richness',
      y = 'Value'
   )



## Fig SI -------------------------------------------------------------------
## 2023-04-27 Investigate the bifurcation in the Specialists scenario
branch = 
   total_function |>
   filter(
      production == 'parallel',
      handling_time == 'flat',
      efficiency == 'increase',
      generalism == 'Specialists', 
      density_dependence == 'Interspecific',
      n > 40,
      n < 50
   )

if(!exists('missing_species')){
   missing_species = NULL
   for(i in 1:nrow(branch)){
      scen = branch$scenario[i]
      seed = branch$seed[i]
      foo =
         paste0(
            'g:/My Drive/Stony Brook University/Conferences/NSF Montreal Workshop/Function group/Data/2023-04-26/scenario-', 
            scen, 
            '_seed-', 
            seed, 
            '.rds'
         ) |>
         readRDS()
      
      missing_species =
         missing_species |>
         bind_rows(
            tibble(
               scenario = scen,
               seed = seed,
               missing =
                  foo |>
                  filter(grepl('N', species)) |>
                  pull(species) |>
                  unique() |>
                  setdiff(x = paste0('N', 1:50))
            ) |>
               mutate(
                  missing = 
                     str_split(missing, 'N', simplify = TRUE)[, 2] |> 
                     as.numeric()
               )
         )
   }
} 

branch =
   branch |>
   inner_join(missing_species) |>
   mutate(richness = factor(n))

figS6 =
   branch |> 
   group_by(scenario, seed) |> 
   summarize(
      missing50 = 50 %in% missing,
      missing49 = 49 %in% missing,
      .groups = 'drop'
   ) |>
   inner_join(branch) |>
   mutate(
      `Species 49` = c('Present', 'Absent')[1 + missing49],
      `Species 50` = c('Present', 'Absent')[1 + missing50],
      `Species richness` = n
   ) |>
   ggplot(aes(missing, func, fill = `Species 50`, shape = `Species 49`)) +
   geom_point() +
   labs(x = 'Missing species', y = 'Function') +
   facet_wrap(~`Species richness`, labeller = label_both) +
   scale_shape_manual(values = c(22, 24)) +
   guides(fill = guide_legend(override.aes = list(color = c('#F8766D', '#619CFF'))))


## Fig5 -------------------------------------------------------------------
## Calculate importance of each dimension of variation - NOT CURRENTLY USED IN MANUSCRIPT

BEF_sign = 
   total_function_summary |>
   filter(n > 1, n < 50) |>
   group_by(across(sampling:efficiency)) |>
   summarize(
      sign = 
         ifelse(
            sd(func) / mean(func) < .01,
            '0',
            ifelse(
               n[func == max(func)] == 49, 
               '+',
               ifelse(
                  n[func == max(func)] == 3,
                  '-',
                  '+-'
               )
            )
         ),
      .groups = 'drop'
   )

importance = 
   BEF_sign |>
   select(-c(uptake_factor, recalcitrance)) |>
   mutate_if(is.character, as.factor) |>
   pivot_longer(density_dependence:efficiency, names_to = 'dimension') |>
   group_by(dimension, value) |>
   summarize(
      pos = sum(sign == '+'),
      neg = sum(sign == '-'),
      mod = sum(sign == '+-'),
      nil = sum(sign == '0'),
      .groups = 'drop'
   ) |>
   pivot_longer(
      pos:nil,
      names_to = 'sign',
      values_to = 'count'
   ) |>
   group_by(dimension, sign) |>
   summarize(
      cv = sd(count) / mean(count),
      .groups = 'drop'
   ) |>
   group_by(dimension) |>
   summarize(
      mean_cv = mean(cv), 
      .groups = 'drop'
   ) |>
   mutate(importance = mean_cv / max(mean_cv)) |>
   mutate(
      dimension = 
         fct_recode(
            dimension,
            !!!c(
               `Niche scenario` = 'generalism',
               `Coregulation scenario` = 'density_dependence',
               `Resource quality` = 'efficiency',
               `Resource recalcitrance` = 'handling_time',
               `By-production architecture` = 'production'
            )
         )
   ) |>
   arrange(desc(importance)) |>
   mutate(dimension = factor(dimension, dimension))

fig5 =
   importance |>
   ggplot(
      aes(
         dimension,
         importance,
         fill = dimension
      )
   ) +
   geom_col(color = 'black') +
   theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank()
   ) +
   ylab('Relative importance')
