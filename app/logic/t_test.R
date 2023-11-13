box::use(
  dplyr[case_when, filter, group_by, mutate, rename],
  echarts4r[e_add_nested, e_bar, e_charts, e_line, e_tooltip, e_grid, e_color,
            e_legend, e_datazoom, e_axis_labels, e_toolbox_feature],
  pwr[pwr.t.test]
)

box::use(
  app/logic/effect[effect_table],
)


# make the grid with calculated power
#' @export
t_table <- function(alpha, n, type, alt) {
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
        type=type,
        alternative=alt,
        power=NULL
      )$power,
      power=round(power, 2),
      d=round(d, 2)
    ) |>
    rename(effect=d)
}


# this function will calculate the sample size at 80% power and user's inputs
#' @export
min_sample <- function(type, alt, effect, alpha) {
  # round up to the next whole person with `ceiling`
  ceiling(
    pwr::pwr.t.test(
      type=type,
      alternative=alt,
      d=effect,
      sig.level=alpha,
      power=0.8,
      n=NULL  # solving for this
    )$n
  )
}


# this function is used to compare the user's vs calculated min sample size
#' @export
t_compare <- function(type, alt, n, effect, alpha) {
  compare <- pwr::pwr.t.test(
    type=type,
    alternative=alt,
    n=n,
    d=effect,
    sig.level=alpha,
    power=NULL
  )
  if (compare$power < 0.8) {
    return("TOO LOW!")
  } else {
    return("Good!")
  }
}
