box::use(
  dplyr[case_when, filter, group_by, mutate],
  echarts4r[e_add_nested, e_bar, e_charts, e_line, e_tooltip, e_grid, e_color,
            e_legend, e_datazoom, e_axis_labels, e_toolbox_feature],
  pwr[pwr.t2n.test]
)

box::use(
  app/logic/chart_utils[left_label_formatter, right_label_formatter],
  app/logic/effect[effect_table],
)


# make the grid with calculated power
#' @export
t_table <- function(alpha, d, group1_n, group2_n, t_type, alt) {
  expand.grid(
    # stop at the large effect size as per table
    d=seq(0.05, effect_table$t_test[3], by=0.05),
    n1=seq(10, n+50, by=1),
    n2=seq(10, n+50, by=1)
  )|>
    mutate(
      power=pwr::pwr.t2n.test(
        sig.level=alpha,
        d=d,
        n1=n1,
        n2=n2,
        type=t_type,
        alternative=alt,
        power=NULL
      )$power,
      power=round(power, 2)
    )
}
