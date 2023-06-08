## Call required libraries ------------------------------
library(tidyverse) ## for tidy data wrangling and piping
library(ggh4x) ## for function facet_grid2()

## Update plot settings ------------------
theme_set(theme_bw())
theme_update(
   panel.grid = element_blank(),
   strip.background = element_blank(),
   aspect.ratio = 1
)

## Read data from GitHub ---------------------
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

results_figS1 = 
  'https://github.com/rafaeldandrea/Pitcher-plant-model/raw/main/data/results_figS1.rds' |>
  url() |>
  readRDS()

missing_species = 
  'https://github.com/rafaeldandrea/Pitcher-plant-model/raw/main/data/results_figS6.rds' |>
  url() |>
  readRDS()

## Merge niche scenario datasets and change value names of simulation scenarios -----------
results =
   results_generalists |>
   bind_rows(results_specialists) |>
   bind_rows(results_gradient) |>
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
            `No co-regulation` = 'none',
            Intraspecific = 'intra',
            Interspecific = 'inter',
            Neutral = 'neutral'
          )
        ),
      efficiency = 
        fct_recode(
          efficiency,
          !!!c(
            Flat = 'flat',
            Increase = 'increase'
          )
        ),
      production =
        fct_recode(
          production,
          !!!c(
            `No by-production` = 'off',
            Nested = 'parallel',
            Serial = 'serial'
          )
        ),
      handling_time = 
        fct_recode(
          handling_time,
          !!!c(
            Decrease = 'decrease',
            Flat = 'flat',
            Increase = 'increase'
          )
        )
   ) |>
   mutate(
      density_dependence = 
        factor(
          density_dependence, 
          levels = 
            c(
              'No co-regulation', 
              'Neutral', 
              'Intraspecific', 
              'Interspecific'
            )
        )
   )
  

## Define default scenario -------------------
default = 
  tibble(
    handling_time == 'Flat',
    efficiency == 'Increase',
    production == 'Nested'
  )

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
   group_by(across(scenario:efficiency)) |>
   summarize(
      func = mean(func),
      .groups = 'drop'
   )

## Fig 1 -------------------
## Schematic representation of model. No code.

## Fig 2 -------------------------------------------------------------------
## Plot BEF for scenarios with increasing resource quality, flat resource recalcitrance, and nested by-production
## include data points for all replicates
fig2 = 
   total_function |>
   inner_join(default)|>
   ggplot(aes(n, func)) +
   geom_point(color = 'grey90') +
   geom_line(
      aes(n, func), 
      data = 
         total_function_summary |>
         inner_join(default)
   ) +
   theme(legend.position = 'none') +
   ggh4x::facet_grid2(density_dependence ~ generalism, scales = 'free_y', independent = 'y') +
   labs(x = 'Species richness', y = 'Total ammonia production')



## Fig 3 -------------------------------------------------------------------
## Plot BEF for scenarios with flat resource recalcitrance
## include data points for all replicates
## Note: Fig3: .ht = 'flat';  FigS2: .ht = 'increase';  FigS3: .ht = 'decrease'
for(.ht in c('Flat', 'Decrease', 'Increase')){
   plot = 
      total_function_summary |>
      filter(handling_time == .ht) |>
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

fig3 = fig3_Flat




## Fig 4 -------------------------------------------------------------------
## Calculate importance of each dimension of variation 
## NOT CURRENTLY USED IN MANUSCRIPT

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

fig4 =
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


## Fig 5 -------------------------------------------------------------------
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
   inner_join(default)|>
   rename(Complementarity = compl.m, Selection = selec.m) |>
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


## Complementarity and Selection by richness ## NOT USED
fig5_alternate = 
   comp_sel |>
   inner_join(default)|>
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



## Fig S1 -----------------------------------
## Observed versus seeded richness
dtf = 
  results_figS1 |>
  group_by(across(scenario:efficiency)) |>
  summarize(
    meanR = mean(max_richness),
    meanS = mean(max_shannon),
    .groups = 'drop'
  ) |>
  mutate(
    efficiency = 
      fct_recode(
        efficiency,
        !!!c(
          Flat = 'flat',
          Increase = 'increase'
        )
      ),
    production =
      fct_recode(
        production,
        !!!c(
          `No by-production` = 'off',
          Nested = 'parallel',
          Serial = 'serial'
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
      ),
    handling_time = 
      fct_recode(
        handling_time,
        !!!c(
          Decrease = 'decrease',
          Flat = 'flat',
          Increase = 'increase'
        )
      )
  )
  

figS1 = 
  dtf |>
  ggplot(aes(n, meanR, color = production, linetype = efficiency)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey') +
  geom_line() +
  facet_grid(handling_time ~ generalism) +
  labs(
    x = 'Seeded richness',
    y = 'Realized richness',
    color = 'By-production\narchitecture',
    linetype = 'Resource\nquality'
  )


## Fig S2 ------------------------
## BEF for scenarios with increasing resource quality
figS2 = fig3_Increase

## Fig S3 -----------------------
## BEF for scenarios with decreasing resource quality
figS3 = fig3_Decrease
## Fig S4 -------------------------------
## Complementarity and selection in other scenarios
figS4 = 
  comp_sel_avg |>
  filter(handling_time == 'Increase') |>
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

## Fig S5 ----------------------------------
## Complementarity and Selection by richness
figS5 = 
  comp_sel|>
  inner_join(default)|>
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


## Fig S6 -------------------------------------------------------------------
## Investigate the bifurcation in the Specialists scenario
branch = 
  missing_species |>
  mutate(richness = factor(n))

figS6 =
  branch |> 
  group_by(scenario, seed) |> 
  summarize(
    missing50 = 50 %in% missing_species,
    missing49 = 49 %in% missing_species,
    .groups = 'drop'
  ) |>
  inner_join(
    branch |>
      select(scenario, n, seed, missing_species, func)
  ) |>
  mutate(
    `Species 49` = c('Present', 'Absent')[1 + missing49],
    `Species 50` = c('Present', 'Absent')[1 + missing50],
    `Species richness` = n
  ) |>
  ggplot(aes(missing_species, func, fill = `Species 50`, shape = `Species 49`)) +
  geom_point() +
  labs(x = 'Missing species', y = 'Function') +
  facet_wrap(~`Species richness`, labeller = label_both) +
  scale_shape_manual(values = c(22, 24)) +
  guides(fill = guide_legend(override.aes = list(color = c('#F8766D', '#619CFF'))))


