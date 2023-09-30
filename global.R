library(shiny)
library(dplyr)
library(tidyr)
library(bslib)
library(pwr)
library(echarts4r)
library(shinydashboardPlus)
library(sass)
library(fresh)

# sass(
#   sass::sass_file('www/styles.scss'),
#   # cache_options = sass_cache_options(FALSE),
#   options = sass_options(output_style = 'compressed'),
#   output = 'www/styles.scss'
# )


pwrTable <-
  expand.grid(
    alpha = c(0.01, 0.025, 0.05),
    effectSize = seq(0.0, 1.0, by = 0.05),
    sampleSize = seq(2, 700, by = 1)
  ) |> 
  mutate(
    power = pwr::pwr.t.test(
      n = sampleSize,
      d = effectSize,
      sig.level = alpha,
      power = NULL
    )$power,
    power = round(power, 2),
  ) |>
  filter(between(power, 0.4, 0.99)) |>
  na.omit()
