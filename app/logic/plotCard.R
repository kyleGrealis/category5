
box::use(
  bslib[card, card_header, card_body],
  dplyr[case_when, filter, group_by, mutate, near],
  echarts4r[e_add_nested, e_bar, e_charts, e_line, e_tooltip, e_grid, e_color,
            e_legend, e_datazoom, e_axis_labels, e_toolbox_feature],
)

box::use(
  app/logic/chart_utils[left_label_formatter, right_label_formatter]
)


# this function will create a card container with a custom heading,
# footer, and plot
#' @export
plotting_cards <- function(headerTextOutput, displayedPlot) {
  card(
    class = "plotting_cards",
    full_screen = TRUE,
    card_header(headerTextOutput),
    card_body(displayedPlot, max_height = "350px"),
  )
}


# this is the left plot for power vs effect size, for given sample size
#' @export
power_effect <- function(data, n, glm=FALSE, unequal=FALSE) {
  # set up for glm data:
  if (glm == TRUE) {
    sample_groups <- c(n)
  # set up for all other tests... filter sample size to be n+/-30
  } else if (n-30 < 5) {
    sample_groups <- c(5, n, n+30)
  } else {
    sample_groups <- c(n-30, n, n+30)
  }
  
  # filter the data based sample groups and if equal or unequal groups
  if (unequal == TRUE) {
    data <- data |> 
    	filter(n2 %in% sample_groups) |> 
      group_by(n2)
  } else {
    data <- data |> 
    	filter(n %in% sample_groups) |> 
      group_by(n)
  }
  
  data |>
    e_charts(effect) |>
    e_line(power) |>
    e_tooltip(
      trigger="item",
      formatter=left_label_formatter
    ) |>
    e_grid(right='15%') |>
    e_color(c("#dfe6e9", "#b2bec3", "#636e72")) |>
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
power_bar <- function(data, n, effect_values, equal=TRUE, ...) {
  # plotting the selected power for sample size at 3 levels of effect size
  
  if (equal == TRUE) {
    plot_ds <- data |> 
      filter(
        n == n,
        effect %in% effect_values
      )
  } else {
    plot_ds <- data |>
      filter(
        n == n,
        n2 == n2,
        effect %in% effect_values
      )
  }
  
  plot_ds |> 
    mutate(
      # custom x-axis labels
      effect=factor(effect, labels=c("Small", "Medium", "Large")),
      # custom bar color
      color=case_when(
        effect == "Small" ~ "#dfe6e9",
        effect == "Medium" ~ "#b2bec3",
        effect == "Large" ~ "#636e72"
      )
    ) |>
    e_charts(effect) |>
    e_bar(power) |>
    e_add_nested("itemStyle", color) |>
    e_tooltip(trigger="item", formatter=right_label_formatter) |>
    e_grid(right='15%') |>
    e_color("#636e72") |>
    e_legend(show=FALSE) |>
    e_datazoom(type='inside') |>
    e_axis_labels(x="Effect \nSize", y="Power") |>
    e_toolbox_feature(feature=c("saveAsImage"))
}

