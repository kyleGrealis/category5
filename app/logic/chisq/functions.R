box::use(
  dplyr[case_when, filter, group_by, mutate],
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
chisq_table <- function(alpha, effect, n, df) {
  expand.grid(
    # stop at the large effect size as per table
    w=seq(0.05, effect_table$chisq_test[3], by=0.05),
    n=seq(10, n+100, by=1)
  )|>
    mutate(
      power=pwr::pwr.chisq.test(
        sig.level=alpha,
        df=df,
        w=w,
        N=n,
        power=NULL
      )$power,
      power=round(power, 2)
}

# plot 1


# plot 2


# this function will calculate the sample size at 80% power and user's inputs
#' @export
min_sample <- function(alpha, effect, n, df) {
  # round up to the next whole person with `ceiling`
  ceiling(
    pwr::pwr.chisq.test(
      sig.level=alpha,
      df=df,
      w=w,
      power=0.8,
      N=NULL  # solving for this
    )$N
  )
}

# this function is used to compare the user's vs calculated min sample size
#' @export
chisq_compare <- function(t_type, alt, n, d, alpha) {
  compare <- pwr::pwr.chisq.test(
    sig.level=alpha,
      df=df,
      w=w,
      N=n,
      power=NULL
  )
  if (compare$power < 0.8) {
    return("TOO LOW!")
  } else {
    return("Good!")
  }
}