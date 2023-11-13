box::use(
  dplyr[case_when, filter, group_by, mutate, rename],
  echarts4r[e_add_nested, e_bar, e_charts, e_line, e_tooltip, e_grid, e_color,
            e_legend, e_datazoom, e_axis_labels, e_toolbox_feature],
  pwr[pwr.t.test],
)

box::use(
  app/logic/effect[effect_table],
)


# make the grid with calculated power
#' @export
corr_table <- function(alpha, n, alt) {
  expand.grid(
    # stop at the large effect size as per table
    r=seq(0.05, effect_table$corr[3], by=0.05),
    n=seq(10, n+100, by=1)
  )|>
    mutate(
      power=pwr::pwr.r.test(
        sig.level=alpha,
        n=n,
        r=r,
        alt=alt,
        power=NULL
      )$power,
      power=round(power, 2),
      r=round(r, 2)
    ) |> 
    	rename(effect=r)
}


# this function will calculate the sample size at 80% power and user's inputs
#' @export
min_sample <- function(alpha, effect, alt) {
  # round up to the next whole person with `ceiling`
  ceiling(
    pwr::pwr.r.test(
      sig.level=alpha,
      r=effect,
      alt=alt,
      power=0.8,
      n=NULL  # solving for this
    )$n
  )
}

# this function is used to compare the user's vs calculated min sample size
#' @export
corr_compare <- function(n, effect, alpha, alt) {
  compare <- pwr::pwr.r.test(
    sig.level=alpha,
    r=effect,
    n=n,
    alt=alt,
    power=NULL
  )
  if (compare$power < 0.8) {
    return("TOO LOW!")
  } else {
    return("Good!")
  }
}
