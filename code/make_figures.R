## Call required libraries ------------------------------
library(tidyverse) ## for tidy data wrangling and piping
library(ggh4x) ## for function facet_grid2()
library(kader) ## for function cuberoot()

## Update plot settings ------------------
theme_set(theme_bw())
theme_update(
   panel.grid = element_blank(),
   strip.background = element_blank(),
   aspect.ratio = 1
)

## Read data from GitHub ---------------------

results_generalists = 
  'https://github.com/rafaeldandrea/Pitcher-plant-model/raw/main/data/2023-06-16_results_generalists.rds' |>
  url() |>
  readRDS()

results_specialists = 
  'https://github.com/rafaeldandrea/Pitcher-plant-model/raw/main/data/2023-06-16_results_specialists.rds' |>
  url() |>
  readRDS()

results_gradient = 
  'https://github.com/rafaeldandrea/Pitcher-plant-model/raw/main/data/2023-06-16_results_gradient.rds' |>
  url() |>
  readRDS()

results_figS1 = 
  'https://github.com/rafaeldandrea/Pitcher-plant-model/raw/main/data/2023-06-16_results_figS1.rds' |>
  url() |>
  readRDS()

results_figS5 = 
  'https://github.com/rafaeldandrea/Pitcher-plant-model/raw/main/data/2023-06-16_results_figS5.rds' |>
  url() |>
  readRDS()



## Merge niche scenario datasets and change value names of simulation scenarios -----------
results =
  results_generalists |>
  bind_rows(results_specialists) |>
  bind_rows(results_gradient) |>
  mutate(
    niches = 
      factor(
        niches,
        levels = 
          c(
            'Generalists',
            'Specialists',
            'Gradient'
          )
      ),
    coregulation =
      factor(
        coregulation,
        levels =
          c(
            'Absent',
            'Neutral',
            'Intraspecific',
            'Interspecific'
          )
      ),
    byproduction =
      factor(
        byproduction,
        c(
          'None',
          'Nested',
          'Serial'
        )
      )
  )
    

## Define default resource quality scenario -------------------
maintext_resource_quality = 'Flat'
si_resource_quality = setdiff(c('Flat', 'Increase'), maintext_resource_quality)

## Calculate total function across consumer species -------------
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
   group_by(across(scenario:quality)) |>
   summarize(
      func = mean(func),
      .groups = 'drop'
   )

## Fig 1 -------------------
## Schematic representation of model. No code.

## Fig 2 -------------------------------------------------------------------
## Plot BEF for scenarios with increasing resource quality, flat handling time, and nested by-production
## include data points for all replicates

f2_scenario =
  tibble(
    handling_time = 'Flat',
    byproduction = 'Nested'
  )

fig2a = 
   total_function |>
   inner_join(f2_scenario)|>
   filter(quality == 'Flat') |>
   ggplot(aes(n, func)) +
   geom_point(color = 'grey90') +
   geom_line(
      aes(n, func), 
      data = 
         total_function_summary |>
         inner_join(f2_scenario) |>
         filter(quality == 'Flat')
   ) +
   theme(legend.position = 'none') +
   ggh4x::facet_grid2(coregulation ~ niches, scales = 'free_y', independent = 'y') +
   labs(x = 'Seeded richness', y = 'Total ammonia production')

fig2b = 
  total_function |>
  inner_join(f2_scenario)|>
  filter(quality == 'Increase') |>
  ggplot(aes(n, func)) +
  geom_point(color = 'grey90') +
  geom_line(
    aes(n, func), 
    data = 
      total_function_summary |>
      inner_join(f2_scenario) |>
      filter(quality == 'Increase')
  ) +
  theme(legend.position = 'none') +
  ggh4x::facet_grid2(coregulation ~ niches, scales = 'free_y', independent = 'y') +
  labs(x = 'Seeded richness', y = 'Total ammonia production')

fig2 = cowplot::plot_grid(fig2a, fig2b, nrow = 1, labels = 'auto')

## Fig 3 -------------------------------------------------------------------
## Plot BEF for all scenarios with increasing resource quality
fig3 = 
  total_function_summary |>
  filter(quality == maintext_resource_quality) |>
  ggplot(aes(n, func, color = handling_time, linetype = byproduction)) +
  geom_line() +
  ggh4x::facet_grid2(coregulation ~ niches, scales = 'free_y', independent = 'y') +
  labs(
    x = 'Seeded richness', 
    y = 'Total ammonia production',
    linetype = 'By-production\narchitecture',
    color = 'Handling\ntime'
  )

## Fig 4 -------------------------------------------------------------------
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
   group_by(across(n:ammonia_capacity)) |>
   summarize(
      compl.m = mean(compl),
      compl.s = sd(compl),
      selec.m = mean(selec),
      selec.s = sd(selec),
      .groups = 'drop'
   )

comp_sel_avg = 
   comp_sel |>
   group_by(across(niches:ammonia_capacity)) |>
   summarize(
      compl.m = mean(compl.m),
      compl.s = sd(compl.m),
      selec.m = mean(selec.m),
      selec.s = sd(selec.m),
      .groups = 'drop'
   )

## Compare scenarios based on complementarity and selection effects 
fig4 = 
  comp_sel_avg |>
  filter(quality == maintext_resource_quality) |>
  mutate(
    C = kader:::cuberoot(compl.m),
    S = kader:::cuberoot(selec.m)
  ) |>
  ggplot(aes(C, S, fill = handling_time, shape = byproduction)) +
  geom_vline(xintercept = 0, color = 'grey') +
  geom_hline(yintercept = 0, color = 'grey') +
  geom_point() +
  facet_grid(coregulation ~ niches) +
  labs(
    x = expression(Complementarity ^ frac(1, 3)), 
    y = expression(Selection ^ frac(1, 3)), 
    fill = 'Handling\ntime', 
    shape = 'By-production\narchitecture'
  ) +
  scale_shape_manual(values = c(22, 21, 24)) +
  guides(
    fill = guide_legend(override.aes = list(color = c('#F8766D','#00BA38','#619CFF')))
  ) +
  coord_cartesian(
    xlim = c(-10, 10),
    ylim = c(-10, 10)
  )


## Fig 5 -------------------------------------------------------------------
## Calculate importance of each dimension of variation 
## SEPARATE CODE

## Fig S1 -----------------------------------
## Observed versus seeded richness
figS1 = 
  results_figS1 |>
  group_by(across(scenario:ammonia_capacity)) |>
  summarize(
    meanR = mean(max_richness),
    meanS = mean(max_shannon),
    .groups = 'drop'
  ) |>
  ggplot(aes(n, meanR, color = byproduction, linetype = quality)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey') +
  geom_line() +
  facet_grid(handling_time ~ niches) +
  labs(
    x = 'Seeded richness',
    y = 'Realized richness',
    color = 'By-production\narchitecture',
    linetype = 'Resource\nquality'
  )


## Fig S2 ------------------------
## BEF for scenarios with flat resource quality
figS2 = 
  total_function_summary |>
  filter(quality == si_resource_quality) |>
  ggplot(aes(n, func, color = handling_time, linetype = byproduction)) +
  geom_line() +
  ggh4x::facet_grid2(coregulation ~ niches, scales = 'free_y', independent = 'y') +
  labs(
    x = 'Seeded richness', 
    y = 'Total ammonia production',
    linetype = 'By-production\narchitecture',
    color = 'Handling\ntime'
  )



## Fig S3 -------------------------------
## Complementarity and selection in scenarios with flat resource quality
figS3 = 
  comp_sel_avg |>
  filter(quality == si_resource_quality) |>
  mutate(
    C = kader:::cuberoot(compl.m),
    S = kader:::cuberoot(selec.m)
  ) |>
  ggplot(aes(C, S, fill = handling_time, shape = byproduction)) +
  geom_vline(xintercept = 0, color = 'grey') +
  geom_hline(yintercept = 0, color = 'grey') +
  geom_point() +
  facet_grid(coregulation ~ niches) +
  labs(
    x = expression(Complementarity ^ frac(1, 3)), 
    y = expression(Selection ^ frac(1, 3)), 
    fill = 'Handling\ntime', 
    shape = 'By-production\narchitecture'
  ) +
  scale_shape_manual(values = c(22, 21, 24, 25)) +
  guides(
    fill = guide_legend(override.aes = list(color = c('#F8766D','#00BA38','#619CFF')))
  ) +
  coord_cartesian(
    xlim = c(-10, 10),
    ylim = c(-10, 10)
  )



## Fig S4 ----------------------------------
## Complementarity and Selection by richness
figS4a = 
  comp_sel|>
  inner_join(f2_scenario)|>
  filter(quality == 'Flat') |>
  mutate(
    Complementarity = compl.m,
    Selection = selec.m,
    DF = Complementarity + Selection) |>
  pivot_longer(Complementarity:DF) |>
  mutate(name = factor(name, levels = c('Complementarity', 'Selection', 'DF'))) |>
  ggplot(aes(n, value, group = name, color = name)) +
  geom_line() +
  ggh4x::facet_grid2(coregulation ~ niches) +
  labs(
    x = 'Seeded richness',
    y = 'Effect',
    color = ''
  ) +
  scale_color_manual(
    labels = 
      c('Complementarity', 'Selection', expression(paste(Delta, 'Function'))),
    values = c('#619CFF', '#F8766D','black')
    ) 
 
figS4b = 
  comp_sel|>
  inner_join(f2_scenario)|>
  filter(quality == 'Increase') |>
  mutate(
    Complementarity = compl.m,
    Selection = selec.m,
    DF = Complementarity + Selection) |>
  pivot_longer(Complementarity:DF) |>
  mutate(name = factor(name, levels = c('Complementarity', 'Selection', 'DF'))) |>
  ggplot(aes(n, value, group = name, color = name)) +
  geom_line() +
  ggh4x::facet_grid2(coregulation ~ niches) +
  labs(
    x = 'Seeded richness',
    y = 'Effect',
    color = ''
  ) +
  scale_color_manual(
    labels = 
      c('Complementarity', 'Selection', expression(paste(Delta, 'Function'))),
    values = c('#619CFF', '#F8766D','black')
  )

legend = cowplot::get_legend(figS4b)

figS4 = cowplot::plot_grid(
  figS4a  + theme(legend.position = 'none'), 
  figS4b  + theme(legend.position = 'none'), 
  legend, 
  nrow = 1, 
  labels = c('a', 'b', ''), 
  rel_widths = c(1, 1, .3)
)

figS7 = 
  comp_sel |>
  filter(quality == si_resource_quality) |>
  group_by(across(niches:ammonia_capacity)) |>
  arrange(n) |>
  mutate(
    cdiff = c(NA, diff(compl.m)),
    sdiff = c(NA, diff(selec.m))
  ) |>
  ungroup() |>
  mutate(
    bef = cdiff + sdiff,
    dominant = ifelse(abs(cdiff) > abs(sdiff), 'C', 'S')
  ) |>
  filter(
    n > 3,
    n < 50,
    bef < 0,
    sign(cdiff) != sign(sdiff)
  ) |>
  ggplot(aes(n, bef, color = handling_time, linetype = byproduction)) +
  geom_line() +
  ggh4x::facet_grid2(coregulation ~ niches, scales = 'free_y', independent = 'y')
  

## Fig S5 -------------------------------------------------------------------
## Investigate the bifurcation in the Specialists scenario
figS5 =
  results_figS6 |>
  mutate(richness = factor(n)) |> 
  group_by(scenario, seed) |> 
  summarize(
    missing50 = 50 %in% missing_species,
    missing49 = 49 %in% missing_species,
    .groups = 'drop'
  ) |>
  inner_join(
    results_figS6 |>
      mutate(richness = factor(n)) |>
      select(scenario, n, seed, missing_species, func)
  ) |>
  mutate(
    `Species 49` = c('Present', 'Absent')[1 + missing49],
    `Species 50` = c('Present', 'Absent')[1 + missing50],
    `Seeded richness` = n
  ) |>
  ggplot(aes(missing_species, func, fill = `Species 50`, shape = `Species 49`)) +
  geom_point() +
  labs(x = 'Missing species', y = 'Function') +
  facet_wrap(~`Seeded richness`, labeller = label_both) +
  scale_shape_manual(values = c(22, 24)) +
  guides(fill = guide_legend(override.aes = list(color = c('#F8766D', '#619CFF'))))


## Table S3 -------------------------
## BEF shape for each scenario
BEF_shape_table = 
  total_function_summary |>
  filter(n > 1, n < 50) |>
  group_by(across(niches:quality)) |>
  summarize(
    BEF_shape = 
      ifelse(
        sd(func) / mean(func) < .03,
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
  ) |>
  select(
    Niches = niches,
    Coregulation = coregulation,
    Handling_time = handling_time,
    Resource_quality = quality,
    Byproduction = byproduction,
    BEF_shape
  ) |>
  arrange(
    Niches,
    Coregulation,
    Handling_time,
    Resource_quality,
    Byproduction
  )
