box::use(
  dplyr[case_when, filter, group_by, mutate, rename],
  echarts4r[e_add_nested, e_bar, e_charts, e_line, e_tooltip, e_grid, e_color,
            e_legend, e_datazoom, e_axis_labels, e_toolbox_feature],
  pwr[ES.h, pwr.2p2n.test],
)

box::use(
  app/logic/chart_utils[left_label_formatter, right_label_formatter],
  app/logic/effect[effect_table],
)


# make the grid with calculated power
#' @export
prop_table <- function(alpha, n1, n2, alt) {
  # set lowest value as 32 because # obs in n1 must be at least 2
  if (n1<32) {
    n1=32
  }
  if (n2<32) {
    n2=32
  }
  
  expand.grid(
    # stop at the large effect size as per table
    h=seq(0.05, effect_table$prop[3], by=0.05),
    n1=n1,
    n2=seq(10, n2+100, by=1)
  )|>
    mutate(
      power=pwr::pwr.2p2n.test(
        sig.level=alpha,
        n1=n1,
        n2=n2,
        h=h,
        alt=alt,
        power=NULL
      )$power,
      power=round(power, 2)
    ) |> 
    	rename(effect=h)
}

# this is the left plot: power vs sample size
#' @export
power_effect <- function(data, n2) {
  # show selected sample size and 30 above and 30 below, min=5
  if (n2-30<2) {
    sample_group = c(2, n2, n2+30)
  } else {
    sample_groups = c(n2-30, n2, n2+30)
  }
  
  data |>
    filter(n2 %in% sample_groups) |>
    group_by(n2) |>
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
power_bar <- function(data, n1, n2) {
  # plotting the selected power for sample size at 3 levels of effect size
  data |>
    filter(
      n1 == n1,
      n2 == n2,
      effect %in% effect_table$prop
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
min_effect <- function(n1, n2, alpha, alt) {
  # round down to maintain mininum group diff
  round(
    pwr::pwr.t2n.test(
      n1=n1,
      n2=n2,
      sig.level=alpha,
      power=0.8,
      alternative=alt,
      h=NULL
    )$h,
    digits = 2
  )
}

# this function is used to compare the user's vs calculated min sample size
#' @export
prop_compare <- function(n1, n2, p1, p2, alpha, alt) {
  compare <- pwr::pwr.2p2n.test(
    sig.level=alpha,
    h=ES.h(p1=p1, p2=p2),
    n1=n1,
    n2=n2,
    alt=alt,
    power=NULL
  )
  if (compare$power < 0.8) {
    return("TOO LOW!")
  } else {
    return("Good!")
  }
}
