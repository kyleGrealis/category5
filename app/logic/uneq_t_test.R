box::use(
  dplyr[case_when, filter, group_by, mutate, rename],
  echarts4r[e_add_nested, e_bar, e_charts, e_line, e_tooltip, e_grid, e_color,
            e_legend, e_datazoom, e_axis_labels, e_toolbox_feature],
  pwr[pwr.t2n.test]
)

box::use(
  app/logic/effect[effect_table],
)


# make the grid with calculated power
#' @export
t2n_table <- function(alpha, n1, n2, alt) {
  # set lowest value as 32 because # obs in n1 must be at least 2
  if (n1<32) {
    n1=32
  }
  if (n2<32) {
    n2=32
  }
  
  expand.grid(
    # stop at the large effect size as per table
    d=seq(0.05, effect_table$t_test[3], by=0.05),
    n1=n1,
    n2=seq(n2-30, n2+100, by=2)
  )|>
    mutate(
      power=pwr::pwr.t2n.test(
        sig.level=alpha,
        d=d,
        n1=n1,
        n2=n2,
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
min_effect <- function(n1, n2, alpha, alt) {
  # round down to maintain mininum group diff
  round(
    pwr::pwr.t2n.test(
      n1=n1,
      n2=n2,
      sig.level=alpha,
      power=0.8,
      alternative=alt,
      d=NULL
    )$d,
    digits = 2
  )
}


# this function is used to compare the user's vs calculated min sample size
#' @export
t2n_compare <- function(alpha, effect, n1, n2, alt) {
  compare <- pwr::pwr.t2n.test(
    sig.level=alpha,
    d=effect,
    n1=n1,
    n2=n2,
    alternative=alt,
    power=NULL
  )
  if (compare$power < 0.8) {
    return("TOO LOW!")
  } else {
    return("Good!")
  }
}
