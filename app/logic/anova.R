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
      power=round(power, 2),
      f=round(f, 2)
    ) |> 
    	rename(effect=f)
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
