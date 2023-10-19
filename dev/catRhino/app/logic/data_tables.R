box::use(
  dplyr[between, filter, mutate],
  pwr[pwr.anova.test, pwr.t.test],
  stats[na.omit]
)

grid <- expand.grid(
  alpha = c(0.01, 0.025, 0.05),
  effectSize = seq(0.0, 2, by = 0.01),
  sampleSize = seq(4, 700 , by = 1)
)

pwrTable <- grid |>
    mutate(
      power = pwr.t.test(
        n = sampleSize,
        d = effectSize,
        sig.level = alpha,
        power = NULL
      )$power,
      power = round(power, 2)
    ) |>
    filter(between(power, 0.6, 0.99)) |>
    na.omit()

# uneq_tTable <- grid |>
#   mutate(
#     power = pwr$pwr.t2n.test(
#       n1 = sampleSize1,
#       n2 = sampleSize2,
#       d = effectSize,
#       sig.level = alpha,
#       alternative = input$alternative_t2n,
#       power = NULL
#     )$power,
#     power = round(power, 2)
#   )
anovaTable <- expand.grid(
  groups = seq(2, 6, by = 1),
  alpha = c(0.01, 0.025, 0.05),
  effectSize = seq(0.0, 2, by = 0.01),
  sampleObs = seq(4, 700 , by = 1)
) |> mutate(
  power = pwr.anova.test(
    k = groups,
    n = sampleObs,
    f = effectSize,
    sig.level = alpha,
    power = NULL
  )$power,
  power = round(power, 2)
) |>
  filter(between(power, 0.6, 0.99)) |>
  na.omit()
