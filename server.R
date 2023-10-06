server <- function(input, output, session) {
  

# plots -------------------------------------------------------------------
  # left plot
  output$power <- renderEcharts4r({
    # validate selected sample size
    if (input$sample < 2 | input$sample > 700 ) {
      validate("Please select a per-group sample size between 2 and 700.")
    }
    
    # from global.R
    left_plot(input$sample, input$alpha)
  })
  
  output$leftCardHeader <- renderText({
    glue::glue("Selected sample size: {input$sample} per group")
  })
  
  # right plot
  output$power2 <- renderEcharts4r({
    # from global.R
    right_plot(input$effect, input$alpha)
  })
  
  output$rightCardHeader <- renderText({
    glue::glue("Selected effect size: {input$effect}")
  })
  
  
  
  # front, left value_box
  output$effectSize <- renderText({input$effect})
  

# flip boxes --------------------------------------------------------------
  # this section is for comparing the selected sample size against the
  # required sample size to achieve >= 80% holding the other variables
  # (alpha, effect size) at a constant. then, a minimal sample size is 
  # calculated and displayed in the middle output box, front side. lastly,
  # the minimal calculated sample size is compared to the user-selected
  # sample size to finally render "sufficient" or "too low"
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
  
  # backside of boxes
  output$sampleCardBack <- renderText({
    "...information coming soon!"
  })
  
  output$backsideRight <- renderText({
    "...information coming soon!"
  })
  
  observeEvent(input$toggle, {
    updateFlipBox("myflipbox")
  })
  
}