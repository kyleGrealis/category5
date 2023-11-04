

# make the grid with calculated power
t_table <- function(t_type, alt, n, d, alpha, ...) {
  expand.grid(
    # stop at the large effect size as per table
    d = seq(0.05, effect_table$means[3], by = 0.05),
    n = seq(10, n+100, by = 1)
  )|>
    mutate(
      power = pwr::pwr.t.test(
        type = t_type,
        alternative = alt,
        n = n,
        d = d,
        sig.level = alpha,
        power = NULL
      )$power,
      power = round(power, 2)
    )
}

# test results of user's selection to compare minimum reqs
t_compare <- function(t_type, alt, n, d, alpha) {
  pwr::pwr.t.test(
    type = t_type,
    alternative = alt,
    n = n,
    d = d,
    sig.level = alpha,
    power = NULL
  )
}



# make the plots ----------------------------------------------------------

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


# make the cards ----------------------------------------------------------

# card 1 is only the selected effect size, so no magic needed

# card 2 is the minimum sample size for 80% power
min_sample <- function(t_type, alt, d, alpha, ...) {
  # round up to the next whole person with `ceiling`
  ceiling(
    pwr::pwr.t.test(
      type = t_type,
      alternative = alt,
      d = d,
      sig.level = alpha,
      power = 0.8,
      n = NULL  # solving for this
    )$n
  )
}

