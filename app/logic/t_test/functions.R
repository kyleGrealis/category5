box::use(
  dplyr[filter, group_by, mutate, tibble],
  echarts4r[e_charts, e_line, e_tooltip, e_grid, e_color,
            e_legend, e_datazoom, e_axis_labels, e_toolbox_feature],
  pwr[pwr.t.test]
)

box::use(
  app/logic/effect[effect_table]
)


# make the grid with calculated power
#' @export
t_table <- function(alpha, d, n, t_type, alt) {
  expand.grid(
    # stop at the large effect size as per table
    d=seq(0.05, effect_table$t_test[3], by=0.05),
    n=seq(10, n+100, by=1)
  )|>
    mutate(
      power=pwr::pwr.t.test(
        sig.level=alpha,
        d=d,
        n=n,
        type=t_type,
        alternative=alt,
        power=NULL
      )$power,
      power=round(power, 2)
    )
}

# this is the left plot: power vs sample size
#' @export
power_effect <- function(data, n, ...) {
  # show selected sample size and 30 above and 30 below, min = 5
  if (n-30 < 5) {
    sample_groups <- c(5, n, n+30)
  } else {
    sample_groups <- c(n-30, n, n+30)
  }
  data |>
    filter(n %in% sample_groups) |>
    group_by(n) |>
    e_charts(d) |>
    e_line(power) |>
    e_tooltip(
      trigger = "item",
      # formatter = left_label_formatter
    ) |>
    e_grid(right = '15%') |>
    e_color(c("#f47321", "#777777", "#005030")) |>
    e_legend(
      left = '5',
      title = list("Sample size")
    ) |>
    e_datazoom(type = 'inside') |>
    e_axis_labels(x = "Effect \nSize", y = "Power") |>
    e_toolbox_feature(feature = c("saveAsImage"))
}

# this is the bar chart: power at the 3 effect sizes for the user-selected sample size
#' @export
power_bar <- function(data, n, d_levels, ...) {
  # plotting the selected power for sample size at 3 levels of effect size
  # d_levels are the S/M/L effect sizes
  data |>
    filter(
      n == n,
      d %in% d_levels
    ) |>
    mutate(
      # custom x-axis labels
      d = factor(d, labels = c("Small", "Medium", "Large")),
      # custom bar color
      color = case_when(
        d == "Small" ~ "#f47321",
        d == "Medium" ~ "#f3f3f3",
        d == "Large" ~ "#005030"
      )
    ) |>
    e_charts(d) |>
    e_bar(power) |>
    e_add_nested("itemStyle", color) |>
    e_tooltip(
      trigger = "item",
      # formatter = left_label_formatter
    ) |>
    e_grid(right = '15%') |>
    e_color("#005030") |>
    e_legend(show = FALSE) |>
    e_datazoom(type = 'inside') |>
    e_axis_labels(x = "Effect \nSize", y = "Power") |>
    e_toolbox_feature(feature = c("saveAsImage"))
}
