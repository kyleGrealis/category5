library(shiny)
library(bslib)

# conditional inputs
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  
  # pwr.t.test
  tabPanel(
    "t",
    numericInput(
      "sample", "Sample size per group",
      min = 20, max = 300, step = 5, value = 100
    ),
    numericInput(
      "effect", "Desired effect size",
      min = 0.1, max = 1.0, step = 0.05, value = 0.5
    ),
    selectInput(
      "tTestType", "t-test type",
      choices = c(
        "Two sample" = "two.sample",
        "One sample" = "one.sample",
        "Paired" = "paired"
      )
    ),
    selectInput(
      "alternative", "Alternative hypothesis type",
      choices = c(
        "Two-sided" = "two.sided",
        "Less than the null" = "less",
        "Greater than the null" = "greater"
      ),
      selected = "two.sided"
    ),
    h6("This tests differences in means."),
    p("Two sample: Is there a difference in the mean income of men and women?"),
    p("One sample: Is the supposed (null) value different than the true value?"),
    p("Paired: Does an exercise program increase speed in the 100m sprint?
      Are the times different (time1 - time2)?")
  ),
  
  # pwr.p.test
  tabPanel(
    "p", 
    numericInput(
      "sampleObs", "Number of tests",
      min = 20, max = 300, step = 5, value = 100
    ),
    numericInput(
      "p1_p", "Null hypothesis proportion", 
      value = 0.55, min = 0, max = 1, step = 0.01
    ),
    numericInput(
      "p2_p", "Alternative hypothesis proportion", 
      value = 0.5, min = 0, max = 1, step = 0.01
    ),
    numericInput(
      "hEffectSize_p", "Desired effect size",
      min = 0, max = 3, value = 0.5, step = 0.01
    ),
    selectInput(
      "alternative", "Alternative hypothesis type",
      choices = c(
        "Two-sided" = "two.sided",
        "Less than the null" = "less",
        "Greater than the null" = "greater"
      ),
      selected = "two.sided"
    ),
    h6("This tests one-sample proportion."),
    p('Example: Is that coin fair? Is the proportion of the outcome 
      "heads" different than the expected 50%?')
  ),
  # pwr.2p.test
  tabPanel(
    "2p",
    numericInput(
      "sampleObs", "Sample size per group",
      min = 20, max = 300, step = 5, value = 100
    ),
    numericInput(
      "p1_2p", "Null hypothesis proportion", 
      value = 0.55, min = 0, max = 1, step = 0.01
    ),
    numericInput(
      "p2_2p", "Alternative hypothesis proportion", 
      value = 0.5, min = 0, max = 1, step = 0.01
    ),
    numericInput(
      "hEffectSize_2p", "Desired effect size",
      min = 0, max = 3, value = 0.5, step = 0.01
    ),
    selectInput(
      "alternative", "Alternative hypothesis type",
      choices = c(
        "Two-sided" = "two.sided",
        "Less than the null" = "less",
        "Greater than the null" = "greater"
      ),
      selected = "two.sided"
    ),
    h6("This tests two-sample proportions."),
    p("Example: Is the proportion of seatbelt use greater in Group A 
      than in Group B?")
  ),
  # pwr.2p2n.test
  tabPanel(
    "2p2n",
    numericInput(
      "sampleN1", "Sample size in group 1",
      min = 20, max = 300, step = 5, value = 100
    ),
    numericInput(
      "sampleN2", "Sample size in group 2",
      min = 20, max = 300, step = 5, value = 100
    ),
    numericInput(
      "p1_2p2n", "Null hypothesis proportion", 
      value = 0.55, min = 0, max = 1, step = 0.01
    ),
    numericInput(
      "p2_2p2n", "Alternative hypothesis proportion", 
      value = 0.5, min = 0, max = 1, step = 0.01
    ),
    numericInput(
      "hEffectSize_2p2n", "Desired effect size",
      min = 0, max = 3, value = 0.5, step = 0.01
    ),
    selectInput(
      "alternative", "Alternative hypothesis type",
      choices = c(
        "Two-sided" = "two.sided",
        "Less than the null" = "less",
        "Greater than the null" = "greater"
      ),
      selected = "two.sided"
    ),
    h6("This tests two-sample proportions of unequal group sizes."),
    p("Example: Is the proportion of seatbelt use greater in Group A 
      than in Group B?")
  ),
  tabPanel(
    "chisq",
    p("Coming soon!")
  ),
  tabPanel(
    "r",
    p("Coming soon!")
  ),
  tabPanel(
    "anov",
    p("Coming soon!")
  ),
  tabPanel(
    "f2",
    p("Coming soon!")
  )
)


shinyApp(
  
  ui <- page_sidebar(
    sidebar = sidebar(
      title = "Plotting controls",
      selectInput(
        "statTestType", "Statistical test:",
        choices = c(
          "t-test (means)" = "t",
          "One sample proportion" = "p",
          "Two sample proportions" = "2p",
          "Unequal n sample proportions" = "2p2n",
          "Chi-square" = "chisq",
          "Correlation" = "r",
          "ANOVA" = "anov",
          "General linear model" = "f2"
        )
      # selectInput("dist", "Distribution", 
      #             choices = c("normal", "uniform", "exponential")
      ),
      # these are the common inputs across all test types
      selectInput(
        "alpha", "Choose your significance level",
        choices = c(0.01, 0.025, 0.05),
        selected = 0.05
      ),
      
      # place the conditional effect size input here???
      
      parameter_tabs,
    ),
    mainPanel(
      p("work in progress")
    )
  ),
  
  server <- function(input, output, session) {
    observeEvent(input$statTestType, {
      updateTabsetPanel(inputId = "params", selected = input$statTestType)
    }) 
    
    # update the effect size for p, 2p, 2p2n
    observeEvent(input$p1_p, {
      updateNumericInput(inputId = "hEffectSize_p", 
                        value = abs(round(pwr::ES.h(input$p1_p, input$p2_p), 2)))
    })  
    observeEvent(input$p2_p, {
      updateNumericInput(inputId = "hEffectSize_p", 
                        value = abs(round(pwr::ES.h(input$p1_p, input$p2_p), 2)))
    })
    observeEvent(input$p1_2p, {
      updateNumericInput(inputId = "hEffectSize_2p", 
                        value = abs(round(pwr::ES.h(input$p1_2p, input$p2_2p), 2)))
    })  
    observeEvent(input$p2_2p, {
      updateNumericInput(inputId = "hEffectSize_2p", 
                        value = abs(round(pwr::ES.h(input$p1_2p, input$p2_2p), 2)))
    })
    observeEvent(input$p1_2p2n, {
      updateNumericInput(inputId = "hEffectSize_2p2n", 
                        value = abs(round(pwr::ES.h(input$p1_2p2n, input$p2_2p2n), 2)))
    })  
    observeEvent(input$p2_2p2n, {
      updateNumericInput(inputId = "hEffectSize_2p2n", 
                        value = abs(round(pwr::ES.h(input$p1_2p2n, input$p2_2p2n), 2)))
    })
    
    # sample <- reactive({
    #   switch(input$dist,
    #          normal = rnorm(input$n, input$mean, input$sd),
    #          uniform = runif(input$n, input$min, input$max),
    #          exponential = rexp(input$n, input$rate)
    #   )
    # })
    # output$hist <- renderPlot(hist(sample()), res = 96)
  }
  
)
