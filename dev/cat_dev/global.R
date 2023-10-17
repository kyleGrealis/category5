library(bslib)
library(shinydashboardPlus)
library(shiny)
library(dplyr)
library(tidyr)
library(pwr)
library(echarts4r)
library(sass)
library(fresh)


pwrTable <- expand.grid(
    alpha = c(0.01, 0.025, 0.05),
    effectSize = seq(0.0, 1.0, by = 0.05),
    sampleSize = seq(2, 700, by = 1)
  ) |> 
  mutate(
    power = pwr::pwr.t.test(
      n = sampleSize,
      d = effectSize,
      sig.level = alpha,
      power = NULL
    )$power,
    power = round(power, 2),
  ) |>
  filter(between(power, 0.4, 0.99)) |>
  na.omit()


# for rendering output plots
# Power vs effect size plot
left_plot <- function(chosenSample, chosenAlpha) {
  pwrTable |>
    filter(
      sampleSize %in% c(chosenSample - 20, chosenSample, chosenSample + 20),
      alpha == chosenAlpha
    ) |>
    mutate(effectSize = round(effectSize, 2)) |> 
    group_by(sampleSize, alpha) |>
    e_charts(effectSize) |>
    e_line(power) |>
    e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(
        "
          function(params){
            return(
              'Power: ' + params.value[1] +
              '<br />Effect size: ' + params.value[0]
            )
          }
          "
      )
    ) |> 
    e_grid(right = '15%') |>
    e_legend(
      left = '5', 
      title = list("Sample size")
    ) |>
    e_datazoom(type = 'inside') |>
    e_y_axis(power) |>
    e_x_axis(effectSize) |> 
    e_axis_labels(x = "Effect \nSize", y = "Power") |> 
    e_toolbox_feature(feature = c("saveAsImage"))
}

# plotting the selected effect size as a line on Power vs sample size plot
right_plot <- function(chosenEffect, chosenAlpha) {
  pwrTable |>
    filter(
      near(effectSize, chosenEffect),
      alpha == chosenAlpha
    ) |>
    group_by(effectSize, alpha) |>
    e_charts(power) |>
    e_line(sampleSize) |>
    e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(
        "
          function(params){
            return(
              'Sample size: ' + params.value[1] +
              '<br />Power: ' + params.value[0]
            )
          }
          "
      )
    ) |>
    e_grid(right = '15%') |>
    e_legend(show = FALSE) |>
    e_datazoom(type = 'inside') |>
    e_y_axis(sampleSize) |>
    e_x_axis(power) |> 
    e_axis_labels(x = "Power", y = "Sample \nSize") |> 
    e_toolbox_feature(feature = c("saveAsImage"))
}


my_sidebar <- sidebar(
  title = "Plotting controls",
  selectInput(
    "alpha", "Choose your significance level",
    choices = c(0.01, 0.025, 0.05),
    selected = 0.05
  ),
  numericInput(
    "effect", "Choose the desired effect size",
    min = 0.1, max = 1.0, step = 0.05, value = 0.5
  ),
  numericInput(
    "sample", "Choose the sample size per group",
    min = 20, max = 300, step = 5, value = 100
  )
)




display_here <- list(
  
  layout_column_wrap(
    width = 1/2,
    
    # left plot
    card(
      full_screen = TRUE,
      card_header(textOutput("leftCardHeader")),
      card_footer(
        "Displaying your desired sample size and ± 20 participants per group."
      ),
      echarts4rOutput("power"),
    ),
    
    # right plot
    card(
      card_header(textOutput("rightCardHeader")),
      card_footer(
        "The effect size line displays the necessary sample size and power."
      ),
      echarts4rOutput("power2"),
    ),
  ), # layout_columns
  
  
  # simple dialogue between plots and flip cards
  p("To achieve at least 80% power, your study will need:"),
  
  
  # flip cards --------------------------------------------------------------
  
  # lower display section with flip cards
  layout_columns(
    
    # left box
    div( # flip-box
      div( # flip-box-inner
        div( # flip-box-front
          value_box(
            title = "Overall measurable effect size",
            value = textOutput("effectSize"),
            showcase = bsicons::bs_icon("graph-up-arrow"),
            theme = "white",
            class = "left-box"
          ),
          class = "flip-box-front"
        ),
        # back side of left card
        div(
          value_box(
            title = "Results are based on the `pwr` package by Clay Ford.
              Refer to this vignette:",
            value = 
              a(
                "Getting started with the pwr package",
                href = "http://cran.nexr.com/web/packages/pwr/vignettes/pwr-vignette.html",
                target = "_blank",
                class = "my-links"
              ),
            showcase = bsicons::bs_icon("graph-up-arrow"),
            theme = "white",
            class = "left-box"
          ),
          class = "flip-box-back"
        ),
        class = "flip-box-inner"
      ),
      class = "flip-box"
    ),
    
    # center box
    div( # flip-box
      div( # flip-box-inner
        div( # flip-box-front
          value_box(
            title = "Minimal sample size per group",
            value = textOutput("minSampleSize"),
            showcase = bsicons::bs_icon("people-fill"),
            theme = "white",
            class = "center-box"
          ),
          class = "flip-box-front"
        ),
        # back side of center card
        div(
          value_box(
            title = "Study design links:",
            value = textOutput("sampleCardBack"),
            showcase = bsicons::bs_icon("people-fill"),
            theme = "white",
            class = "center-box"
          ),
          class = "flip-box-back"
        ),
        class = "flip-box-inner"
      ),
      class = "flip-box"
    ),
    
    # right box
    value_box(
      title = "Your proposed study power will be:",
      value = textOutput("sampleResult"),
      showcase = bsicons::bs_icon("bullseye"),
      theme = "black",
      class = "right-box"
    )
  ) # layout_columns
)
