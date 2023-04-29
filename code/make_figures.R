library(tidyverse) ## for tidy data wrangling and piping
library(ggh4x) ## for function facet_grid2()

theme_set(theme_bw())
theme_update(
   panel.grid = element_blank(),
   strip.background = element_blank(),
   aspect.ratio = 1
)

data_folder = '2023-03-30/'

dir_path = 
   paste0('g:/My Drive/Stony Brook University/Conferences/NSF Montreal Workshop/Function group/Data/', data_folder)

results = readRDS(paste0(dir_path, 'results.rds'))

## Total function across consumer species
total_function =
   results |>
   group_by(across(scenario:time)) |>
   summarize(
      func = sum(value),
      .groups = 'drop'
   )

## Average total function across scenario replicates
total_function_summary =
   total_function |>
   group_by(across(scenario:efficiency)) |>
   summarize(
      func = mean(func),
      .groups = 'drop'
   )

## Plot BEF for scenarios with increasing resource quality, flat resource recalcitrance, and nested by-production
## include data points for all replicates
fig2_main = 
   total_function |>
   filter(
      efficiency == 'increase',
      handling_time == 'flat',
      production == 'parallel'
   ) |>
   ggplot(aes(n, func)) +
   geom_point(color = 'grey90') +
   geom_line(
      aes(n, func, color = density_dependence), 
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

## Plot BEF for scenarios with flat resource recalcitrance
## include data points for all replicates
## Note: change handling_time == 'flat' to 'increase' or 'decrease' for the other scnearios
fig3_main = 
   total_function_summary |>
   filter(handling_time == 'flat') |>
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
comp_sel1 = 
   Hector |>
   filter(n > 1) |>
   group_by(across(n:efficiency)) |>
   summarize(
      compl.m = mean(compl),
      compl.s = sd(compl),
      selec.m = mean(selec),
      selec.s = sd(selec),
      .groups = 'drop'
   )

comp_sel = 
   Hector |>
   filter(n > 1) |>
   group_by(across(sampling:efficiency)) |>
   summarize(
      compl.m = mean(compl),
      compl.s = sd(compl),
      selec.m = mean(selec),
      selec.s = sd(selec),
      .groups = 'drop'
   )

## Compare scenarios based on complementarity and selection effects 
fig4_main = 
   comp_sel |>
   filter(
      handling_time == 'flat',
      efficiency == 'increase',
      production == 'parallel'
   ) |>
   ggplot(aes(compl.m, selec.m, color = generalism, shape = density_dependence)) +
   geom_vline(xintercept = 0, color = 'grey') +
   geom_hline(yintercept = 0, color = 'grey') +
   geom_errorbar(
      aes(
         x = compl.m,
         ymin = selec.m - selec.s,
         ymax = selec.m + selec.s
      )
   ) +
   geom_errorbarh(
      aes(
         xmin = compl.m - compl.s,
         xmax = compl.m + compl.s
      )
   ) +
   geom_point() +
   labs(
      x = 'Complementarity', 
      y = 'Selection', 
      color = 'Uptake scenario', 
      shape = 'Density dependence'
   )

## Complementarity and Selection by richness
fig4_alternate = 
   comp_sel1|>
   rename(Complementarity = compl.m, Selection = selec.m) |>
   pivot_longer(c(Complementarity, Selection), names_to = 'effect') |>
   filter(
      handling_time == 'flat',
      efficiency == 'increase',
      production == 'parallel'
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
               Interspecific = 'inter'
            )
         )
   ) |>
   ggplot(aes(n, value, color = generalism, linetype = density_dependence)) +
   geom_line() +
   facet_grid(~effect) +
   labs(
      x = 'Species richness',
      y = 'Value',
      color = 'Niche\nscenario',
      linetype = 'Co-regulation\nscenario'
   )

## Complementarity and Selection by richness
fig4_alternate2 = 
   comp_sel1|>
   filter(
      handling_time == 'flat',
      efficiency == 'increase',
      production == 'parallel'
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
               Interspecific = 'inter'
            )
         )
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

fig4_SI = 
   comp_sel |>
   filter(
      handling_time == 'flat'
   ) |>
   ggplot(aes(compl.m, selec.m, color = generalism, shape = density_dependence)) +
   geom_vline(xintercept = 0, color = 'grey') +
   geom_hline(yintercept = 0, color = 'grey') +
   geom_point() +
   facet_grid(efficiency ~ production) +
   labs(
      x = 'Complementarity', 
      y = 'Selection', 
      color = 'Uptake scenario', 
      shape = 'Density dependence'
   )
