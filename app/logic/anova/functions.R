box::use(
  dplyr[case_when, filter, group_by, mutate, rename],
  echarts4r[e_add_nested, e_bar, e_charts, e_line, e_tooltip, e_grid, e_color,
            e_legend, e_datazoom, e_axis_labels, e_toolbox_feature],
  pwr[pwr.t.test]
)

box::use(
  app/logic/chart_utils[left_label_formatter, right_label_formatter],
  app/logic/effect[effect_table],
)


# make the grid with calculated power
#' @export
anova_table <- function(alpha, n, k) {
  expand.grid(
    # stop at the large effect size as per table
    f=seq(0.05, effect_table$anova[3], by=0.05),
    n=seq(10, n+100, by=1)
  )|>
    mutate(
      power=pwr::pwr.anova.test(
        sig.level=alpha,
        f=f,
        n=n,
        k=k,
        power=NULL
      )$power,
      power=round(power, 2)
    ) |> 
    	rename(effect=f)
}

# this is the left plot: power vs sample size
#' @export
power_effect <- function(data, n) {
  # show selected sample size and 30 above and 30 below, min=5
  if (n-30 < 5) {
    sample_groups <- c(5, n, n+30)
  } else {
    sample_groups <- c(n-30, n, n+30)
  }
  data |>
    filter(n %in% sample_groups) |>
    group_by(n) |>
    e_charts(effect) |>
    e_line(power) |>
    e_tooltip(
      trigger="item",
      formatter=left_label_formatter
    ) |>
    e_grid(right='15%') |>
    e_color(c("#f47321", "#777777", "#005030")) |>
    e_legend(
      left='5',
      title=list("Sample size")
    ) |>
    e_datazoom(type='inside') |>
    e_axis_labels(x="Effect \nSize", y="Power") |>
    e_toolbox_feature(feature=c("saveAsImage"))
}

# this is the bar chart: power at the 3 effect sizes for the user-selected sample size
#' @export
power_bar <- function(data, n) {
  # plotting the selected power for sample size at 3 levels of effect size
  data |>
    filter(
      n == n,
      effect %in% effect_table$anova
    ) |>
    mutate(
      # custom x-axis labels
      effect=factor(effect, labels=c("Small", "Medium", "Large")),
      # custom bar color
      color=case_when(
        effect == "Small" ~ "#f47321",
        effect == "Medium" ~ "#f3f3f3",
        effect == "Large" ~ "#005030"
      )
    ) |>
    e_charts(effect) |>
    e_bar(power) |>
    e_add_nested("itemStyle", color) |>
    e_tooltip(
      trigger="item",
      # formatter=right_label_formatter
    ) |>
    e_grid(right='15%') |>
    e_color("#005030") |>
    e_legend(show=FALSE) |>
    e_datazoom(type='inside') |>
    e_axis_labels(x="Effect \nSize", y="Power") |>
    e_toolbox_feature(feature=c("saveAsImage"))
}

# this function will calculate the sample size at 80% power and user's inputs
#' @export
min_sample <- function(alpha, effect, k) {
  # round up to the next whole person with `ceiling`
  ceiling(
    pwr::pwr.anova.test(
      k=k,
      f=effect,
      sig.level=alpha,
      power=0.8,
      n=NULL  # solving for this
    )$n
  )
}


# this function is used to compare the user's vs calculated min sample size
#' @export
anova_compare <- function(alpha, effect, n, k) {
  compare <- pwr::pwr.anova.test(
    k=k,
    n=n,
    f=effect,
    sig.level=alpha,
    power=NULL
  )
  if (compare$power < 0.8) {
    return("TOO LOW!")
  } else {
    return("Good!")
  }
}
