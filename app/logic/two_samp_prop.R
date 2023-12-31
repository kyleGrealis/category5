box::use(
  dplyr[case_when, filter, group_by, mutate, rename],
  echarts4r[e_add_nested, e_bar, e_charts, e_line, e_tooltip, e_grid, e_color,
            e_legend, e_datazoom, e_axis_labels, e_toolbox_feature],
  pwr[ES.h, pwr.2p.test],
)

box::use(
  app/logic/effect[effect_table],
)


# make the grid with calculated power
#' @export
prop_table <- function(alpha, n, alt) {
  expand.grid(
    # stop at the large effect size as per table
    h=seq(0.05, effect_table$prop[3], by=0.05),
    n=seq(10, n+100, by=1)
  )|>
    mutate(
      power=pwr::pwr.2p.test(
        sig.level=alpha,
        n=n,
        h=h,
        alt=alt,
        power=NULL
      )$power,
      power=round(power, 2),
      h=round(h, 2)
    ) |> 
    	rename(effect=h)
}


# this function will calculate the sample size at 80% power and user's inputs
#' @export
min_sample <- function(alpha, p1, p2, alt) {
  # round up to the next whole person with `ceiling`
  ceiling(
    pwr::pwr.2p.test(
      sig.level=alpha,
      h=ES.h(p1=p1, p2=p2),
      alt=alt,
      power=0.8,
      n=NULL  # solving for this
    )$n
  )
}

# this function is used to compare the user's vs calculated min sample size
#' @export
prop_compare <- function(n, p1, p2, alpha, alt) {
  compare <- pwr::pwr.2p.test(
    sig.level=alpha,
    h=ES.h(p1=p1, p2=p2),
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
