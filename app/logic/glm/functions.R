box::use(
  dplyr[case_when, filter, group_by, mutate, rename],
  echarts4r[e_add_nested, e_bar, e_charts, e_line, e_tooltip, e_grid, e_color,
            e_legend, e_datazoom, e_axis_labels, e_toolbox_feature],
  pwr[pwr.t.test],
)

box::use(
  app/logic/chart_utils[left_label_formatter, right_label_formatter],
  app/logic/effect[effect_table],
)


# make the grid with calculated power
#' @export
glm_table <- function(alpha, u, n) {
  expand.grid(
    # stop at the large effect size as per table
    f2=seq(0.01, 0.35, by=0.01),
    n=seq(10, n+200, by=5),
    v=n-u-1,
    u=u
  ) |>
    mutate(
      power=pwr::pwr.f2.test(
        sig.level=alpha,
        u=u,
        v=v,
        f2=f2,
        power=NULL
      )$power,
      power=round(power, 2),
      f2=round(f2, 2)
    ) |>
    rename(effect=f2)
}

# this is the left plot: power vs sample size
#' @export
power_effect <- function(data, n) {
  # this is different than the others... only plotting for 1 sample size
  data |>
    filter(n == n) |>
    group_by(n) |>
    e_charts(effect) |>
    e_line(power) |>
    e_tooltip(
      trigger="item",
      formatter=left_label_formatter
    ) |>
    e_grid(right='15%') |>
    e_color("#005030") |>
    e_legend(
      show=FALSE
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
      effect %in% effect_table$glm
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
min_sample <- function(alpha, effect, u) {
  # round up to the next whole person with `ceiling`
  ceiling(
    pwr::pwr.f2.test(
      sig.level=alpha,
      f2=effect,
      u=u,
      power=0.8,
      v=NULL  # solving for this
    )$v
  )
}

# this function is used to compare the user's vs calculated min sample size
#' @export
glm_compare <- function(n, effect, alpha, u) {
  
  v=n-u-1
  
  compare <- pwr::pwr.f2.test(
    sig.level=alpha,
    f2=effect,
    v=v,
    u=u,
    power=NULL
  )
  if (compare$power < 0.8) {
    return("TOO LOW!")
  } else {
    return("Good!")
  }
}
