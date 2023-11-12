
box::use(
  bslib[card, card_header, card_body],
  dplyr[filter, group_by, mutate, near],
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
power_effect <- function(data, n) {
  # show selected sample size and 30 above and 30 below, min=5
  if (n-30 < 5) {
    sample_groups <- c(5, n, n+30)
  } else {
    sample_groups <- c(n-30, n, n+30)
  }
  data |>
    filter(n %in% sample_groups) |>
    group_by(n) |>
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
power_bar <- function(data, n, type) {
  # plotting the selected power for sample size at 3 levels of effect size
  data |>
    filter(
      n == n,
      # effect %in% effect_table$chisq
      # this will grab the values from the effect table
      effect %in% type
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
    e_tooltip(trigger="item") |>
    e_grid(right='15%') |>
    e_color("#005030") |>
    e_legend(show=FALSE) |>
    e_datazoom(type='inside') |>
    e_axis_labels(x="Effect \nSize", y="Power") |>
    e_toolbox_feature(feature=c("saveAsImage"))
}