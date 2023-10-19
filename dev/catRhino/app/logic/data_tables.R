# box::use(
#   dplyr[between, filter, mutate],
#   pwr
# )


# pwrTable <- expand.grid(
#   alpha = c(0.01, 0.025, 0.05),
#   effectSize = seq(0.0, 1.0, by = 0.05),
#   sampleSize = seq(2, 700, by = 1)
# ) |>
#   mutate(
#     power = pwr$pwr.t.test(
#       n = sampleSize,
#       d = effectSize,
#       sig.level = alpha,
#       power = NULL
#     )$power,
#     power = round(power, 2),
#   ) |>
#   filter(between(power, 0.4, 0.99)) |>
#   na.omit()

pwrTable <- expand.grid(
  alpha = c(0.01, 0.025, 0.05),
  effectSize = seq(0.0, 2, by = 0.01),
  sampleSize = seq(4, 700 , by = 1)
)

