library(shiny)
library(bslib)
library(tidyverse)
library(pwr)

# conditional inputs
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  
  # pwr.t.test
  tabPanel(
    "t",
    numericInput(
      "sample", "Sample size per group",
      min = 20, max = 700, step = 5, value = 100
    ),
    numericInput(
      "effectSize", "Desired effect size",
      min = 0.1, max = 3.0, step = 0.1, value = 0.5
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
      "alternative_t", "Alternative hypothesis type",
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
  
  # pwr.t2n.test
  tabPanel(
    "t2n",
    numericInput(
      "sampleN1_t2n", "Sample size in group 1",
      min = 20, max = 700, step = 5, value = 50
    ),
    numericInput(
      "sampleN2_t2n", "Sample size in group 2",
      min = 20, max = 700, step = 5, value = 75
    ),
    numericInput(
      "effectSize_d", "Desired effect size",
      min = 0.1, max = 3, step = 0.1, value = 0.5
    ),
    selectInput(
      "alternative_t2n", "Alternative hypothesis type",
      choices = c(
        "Two-sided" = "two.sided",
        "Less than the null" = "less",
        "Greater than the null" = "greater"
      ),
      selected = "two.sided"
    ),
    h6("This tests differences in means with unequal sample sizes."),
    p("Example: Is the mean income in Group 1 (with 50 participants) different 
    than the mean income in Group 2 (with 75 participants)?")
  ),
  
  # pwr.p.test
  tabPanel(
    "p", 
    numericInput(
      "sampleObs", "Number of tests",
      min = 20, max = 700, step = 5, value = 100
    ),
    numericInput(
      "p1_p", "Null hypothesis proportion", 
      min = 0, max = 1, step = 0.05, value = 0.55 
    ),
    numericInput(
      "p2_p", "Alternative hypothesis proportion", 
      min = 0, max = 1, step = 0.05, value = 0.5
    ),
    numericInput(
      "hEffectSize_p", "Desired effect size",
      min = 0, max = 3, step = 0.1, value = 0.5
    ),
    selectInput(
      "alternative_p", "Alternative hypothesis type",
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
      min = 20, max = 700, step = 5, value = 100
    ),
    numericInput(
      "p1_2p", "Null hypothesis proportion", 
      min = 0, max = 1, step = 0.05, value = 0.55
    ),
    numericInput(
      "p2_2p", "Alternative hypothesis proportion", 
      min = 0, max = 1, step = 0.05, value = 0.5
    ),
    numericInput(
      "hEffectSize_2p", "Desired effect size",
      min = 0, max = 3, step = 0.1, value = 0.5
    ),
    selectInput(
      "alternative_2p", "Alternative hypothesis type",
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
      "sampleN1_2p2n", "Sample size in group 1",
      min = 20, max = 700, step = 5, value = 100
    ),
    numericInput(
      "sampleN2_2p2n", "Sample size in group 2",
      min = 20, max = 700, step = 5, value = 100
    ),
    numericInput(
      "p1_2p2n", "Null hypothesis proportion", 
      min = 0, max = 1, step = 0.05, value = 0.55
    ),
    numericInput(
      "p2_2p2n", "Alternative hypothesis proportion", 
      min = 0, max = 1, step = 0.05, value = 0.5, 
    ),
    numericInput(
      "hEffectSize_2p2n", "Desired effect size",
      min = 0, max = 3, step = 0.1, value = 0.5
    ),
    selectInput(
      "alternative_2p2n", "Alternative hypothesis type",
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
  
  # pwr.anova.test
  tabPanel(
    "anova",
    numericInput(
      "sampleObs", "Number of tests",
      min = 20, max = 700, step = 5, value = 100
    ),
    numericInput(
      "k", "Number of groups (classes)",
      min = 2, max = 6, step = 1, value = 2
    ),
    numericInput(
      "effectSize_f", "Desired effect size",
      min = 0, max = 3, step = 0.1, value = 0.5
    ),
    h6("This tests differences in means across k groups."),
    p("Example: Assuming equal sample sizes per group, is the mean weight
    loss from k exercise groups different?")
  ),
  
  # pwr.r.test
  tabPanel(
    "r",
    p("The r coefficient is the effect size for this test.",
      style = "font-size = 0.7em;"),
    numericInput(
      "r", "Linear correlation coefficient",
      min = -1, max = 1, step = 0.05, value = 0.1
    ),
    numericInput(
      "sampleObs", "Total sample size",
      min = 5, max = 700, step = 5, value = 100
    ),
    selectInput(
      "alternative_r", "Alternative hypothesis type",
      choices = c(
        "Two-sided" = "two.sided",
        "Less than the null" = "less",
        "Greater than the null" = "greater"
      ),
      selected = "two.sided"
    ),
    h6("This tests the estimated correlation."),
    p("Example: Is the correlation between the number of hours a student studies 
      and final test average significantly different than 0?")
  ),
  
  # pwr.chisq.test
  tabPanel(
    "chisq",
    numericInput(
      "p1_sq", "Null hypothesis proportion", 
      min = 0, max = 1, step = 0.05, value = 0.55
    ),
    numericInput(
      "p2_sq", "Alternative hypothesis proportion", 
      min = 0, max = 1, step = 0.05, value = 0.5, 
    ),
    numericInput(
      "wEffectSize", "Desired effect size",
      min = 0, max = 3, step = 0.1, value = 0.5
    ),
    numericInput(
      "sampleObs_csq", "Total sample size",
      min = 20, max = 700, step = 5, value = 100
    ),
    numericInput(
      "df_sq", "Number of categories tested",
      min = 2, max = 10, value = 2, step = 1
    ),
    h6("This tests goodness of fit group proportions."),
    p("Example: Is the proportion of adherence to exercise habits in Group A
      different from the proportion(s) in all other groups?")
  ),
  ## try to figure out how to make a dynamic interface for doing test of association
  # need: to fill in cells that could vary in nrow() and ncol()
  
  # pwr.f2.test
  tabPanel(
    "f2",
    numericInput(
      "sampleObs_f2", "Sample size",
      min = 1, max = 700, step = 1, value = 50
    ),
    numericInput(
      "u", "Number of variables in the model", 
      min = 1, max = 100, step = 1, value = 1, 
    ),
    numericInput(
      "f2EffectSize", "Desired effect size",
      min = 0, max = 3, step = 0.1, value = 0.5
    ),
    h6("This tests regression coefficients."),
    p("Example: Are any regression coefficients statistically
    distinguishable from 0?")
  ),
)


shinyApp(
  
  ui <- page_sidebar(
    sidebar = sidebar(
      title = "Plotting controls",
      selectInput(
        "statTestType", "Statistical test:",
        choices = c(
          "t-test (means)" = "t",
          "t-test, unequal sample sizes" = "t2n",
          "One sample proportion" = "p",
          "Two sample proportions" = "2p",
          "Unequal n sample proportions" = "2p2n",
          "ANOVA" = "anova",
          "Correlation" = "r",
          "Chi-square" = "chisq",
          "General linear model" = "f2"
        )
      ),
      
      # these are the common inputs across all test types
      selectInput(
        "alpha", "Significance level",
        choices = c(0.01, 0.025, 0.05),
        selected = 0.05
      ),
      
      # display conditional inputs here
      parameter_tabs,
    ),
    
    mainPanel(
      p("work in progress"),
      tableOutput("pwrOutputTable")
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
    observeEvent(input$p1_sq, {
      updateNumericInput(inputId = "wEffectSize", 
                        value = abs(round(pwr::ES.w1(input$p1_sq, input$p2_sq), 2)))
    })  
    observeEvent(input$p2_sq, {
      updateNumericInput(inputId = "wEffectSize", 
                        value = abs(round(pwr::ES.w1(input$p1_sq, input$p2_sq), 2)))
    })
    observeEvent(input$sampleObs_f2, {
      if(input$sampleObs_f2 - input$u - 1 < 1){
        # sample obs MUST be 2+ > than numerator df
        updateNumericInput(inputId = "u",
                           value = input$sampleObs_f2 - 2)
      } else {
        # do nothing
        NULL
      }
    })
    observeEvent(input$u, {
      if(input$sampleObs_f2 - input$u - 1 < 1){
        # sample obs MUST be 2+ > than numerator df
        updateNumericInput(inputId = "sampleObs_f2",
                           value = input$u + 2)
      } else {
        # do nothing
        NULL
      }
    })
    
    
    # create a grid of possible combinations of sig level, effect, sample size
    my_table <-
      expand.grid(
        alpha = c(0.01, 0.025, 0.05),
        effectSize = seq(0.0, 2, by = 0.01),
        sampleSize = seq(4, 700 , by = 1)
      )
    
    # make a 2 sample size table
    # my_table_2n <-
    #   expand.grid(
    #     alpha = c(0.01, 0.025, 0.05),
    #     effectSize = seq(0.0, 3.0, by = 0.01),
    #     sampleSize1 = seq(2, 700 , by = 1),
    #     sampleSize2 = seq(2, 700 , by = 1)
    #   )
    
    # make a table for f2
    my_f2_table <- reactive({
      expand.grid(
        alpha = c(0.01, 0.025, 0.05),
        num_df = seq(1, 100, by = 1),
        effectSize = seq(0, 2, by = 0.01) 
      ) |> 
        mutate(denom_df = input$sampleObs_f2 - num_df - 1) |> 
        filter(denom_df >= 1)
    })
      
      
    
    # create function to process power calculation
    power_calc <- function(){
      NULL
    }
    
    # use created function inside a reactive with switch()
    
    pwrTable <- reactive({
      switch(input$statTestType,
             "t" = my_table |> 
               mutate(
                 power = pwr.t.test(
                   n = sampleSize,
                   d = effectSize,
                   sig.level = alpha,
                   alternative = input$alternative_t,
                   type = input$tTestType,
                   power = NULL
                 )$power,
                 power = round(power, 2)
               ),
             
             # "t2n" = my_table_2n |> 
             #   mutate(
             #     power = pwr.t2n.test(
             #       n1 = sampleSize1,
             #       n2 = sampleSize2,
             #       d = effectSize,
             #       sig.level = alpha,
             #       alternative = input$alternative_t2n,
             #       power = NULL
             #     )$power,
             #     power = round(power, 2)
             #   ),
             
             "p" = my_table |> 
               mutate(
                 power = pwr.p.test(
                   n = sampleSize,
                   h = effectSize,
                   sig.level = alpha,
                   alternative = input$alternative_p,
                   power = NULL
                 )$power,
                 power = round(power, 2)
               ),
             
             "2p" = my_table |> 
               mutate(
                 power = pwr.2p.test(
                   n = sampleSize,
                   h = effectSize,
                   sig.level = alpha,
                   alternative = input$alternative_2p,
                   power = NULL
                 )$power,
                 power = round(power, 2)
               ),
             
             # "2p2n" = my_table_2n |> 
             #   mutate(
             #     power = pwr.2p2n.test(
             #       n1 = sampleSize1,
             #       n2 = sampleSize2,
             #       h = effectSize,
             #       sig.level = alpha,
             #       alternative = input$alternative_2p2n,
             #       power = NULL
             #     )$power,
             #     power = round(power, 2)
             #   ),
             
             "anova" = my_table |> 
               mutate(
                 power = pwr.anova.test(
                   k = input$k,
                   n = sampleSize,
                   f = effectSize,
                   sig.level = alpha,
                   power = NULL
                 )$power,
                 power = round(power, 2)
               ),
             
             "r" = my_table |> 
               mutate(
                 power = pwr.r.test(
                   n = sampleSize,
                   r = effectSize,
                   sig.level = alpha,
                   alternative = input$alternative_r,
                   power = NULL
                 )$power,
                 power = round(power, 2)
               ) |> 
               na.omit(),
             
             "chisq" = my_table |> 
               mutate(
                 power = pwr.chisq.test(
                   N = sampleSize,
                   w = effectSize,
                   sig.level = alpha,
                   df = input$df_sq,
                   power = NULL
                 )$power,
                 power = round(power, 2)
               ),
             
             "f2" = my_f2_table() |> 
               mutate(
                 power = pwr.f2.test(
                   u = num_df,
                   # v = n-u-1
                   v = denom_df,
                   f2 = effectSize,
                   sig.level = alpha,
                   power = NULL
                 )$power,
                 power = round(power, 2)
               ) |> 
               filter(denom_df >= 0)
      )
    })
    
    output$pwrOutputTable <- renderTable({
      tail(pwrTable(), 10)
    })
  }
  
)
