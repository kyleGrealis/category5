server <- function(input, output, session) {
  
  output$backsideLeft <- renderText({
    "Effect size is based on Cohen's d."
  })
  
  output$backsideCenter <- renderText({
    "...information coming soon!"
  })
  
  output$backsideRight <- renderText({
    "...information coming soon!"
  })
  
  observeEvent(input$toggle, {
    updateFlipBox("myflipbox")
  })
  
  # left plot
  output$power <- renderEcharts4r({
    pwrTable |>
      filter(
        sampleSize %in% c(input$sample - 20, input$sample, input$sample + 20),
        alpha == input$alpha
      ) |>
      group_by(sampleSize, alpha) |>
      e_charts(effectSize) |>
      e_line(power) |>
      e_tooltip(trigger = "axis") |>
      e_grid(right = '15%') |>
      e_legend(
        orient = 'vertical', right = '5', top = '45%',
        label = "Sample size"
      ) |>
      e_datazoom(type = 'inside') |>
      e_y_axis(power) |>
      e_x_axis(effectSize) |> 
      e_axis_labels(x = "Effect \nSize", "Power")
  })
  
  # right plot
  output$power2 <- renderEcharts4r({
    pwrTable |>
      filter(
        near(effectSize, input$effect),
        alpha == input$alpha
      ) |>
      group_by(effectSize, alpha) |>
      e_charts(power) |>
      e_line(sampleSize) |>
      e_tooltip(trigger = "axis") |>
      e_grid(right = '15%') |>
      e_legend(show = FALSE) |>
      e_datazoom(type = 'inside') |>
      e_y_axis(sampleSize) |>
      e_x_axis(power) |> 
      e_axis_labels(x = "Power", y = "Sample Size")
  })
  
  # front, left value_box
  output$effectSize <- renderText({input$effect})
  
  comparisonTable <- reactive({
    pwrTable |> 
      filter(
        power >= 0.8,                    # for minimal acceptable power
        alpha == input$alpha,            # user input
        effectSize >= input$effect,      # user input      
      ) |> 
      arrange(power, effectSize)
  })
  
  # front, middle value_box
  output$minSampleSize <- renderText({
    comparisonTable() |> 
      pull(sampleSize) |> 
      head(1)
  })
  
  # front, right value_box
  studySampleNeeded <- reactive({
    pwrTable |> 
      filter(
        power >= 0.8,
        alpha == input$alpha,
        effectSize >= input$effect
      ) |> 
      arrange(power, effectSize) |> 
      pull(sampleSize) |> 
      head(1)
  })
  
  output$sampleResult <- renderText({
    if (studySampleNeeded() <= input$sample) {
      "SUFFICIENT"
    } else {
      "TOO LOW!"
    }
  })
  
}