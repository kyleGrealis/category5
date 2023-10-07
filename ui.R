
ui <- page_sidebar(

  # include SCSS file
  tags$head(
    tags$style(sass(sass_file("www/styles.scss")))
  ),
  

# welcome modal -----------------------------------------------------------

  # welcome popup modal
  # modalDialog(
  #   h5("This statistical power calculator and plotting tool is
  #      100% interactive!"),
  #   br(),
  #   h5("Dynamically change the plots and view the impact on study power when 
  #      changing significance, effect size, and sample size. Click download 
  #      icon inside the plot output to save the image."),
  #   br(),
  #   h5("Hover your pointer to flip the lower info cards for package links
  #      and study design resources."),
  #   title = "CAT 5: A Hurricane's Power Calculator & Plotting Tool",
  #   size = "l",
  #   easyClose = FALSE,
  # ),
  

# title -------------------------------------------------------------------

  # application title
  title = "CAT 5: A Hurricane's Power Calculator & Plotting Tool",


# sidebar -----------------------------------------------------------------

  # sidebar / input controls
  sidebar = sidebar(
    title = "Plotting controls",
    selectInput(
      "test",
      "For which statistical test would you like to calculate power?",
      choices = c("t-test", "More coming soon!"),
      selected = "t-test"
    ),
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
    ),
    
    # footer-- link to GitHub page
    div(
      p(
        em("Written & developed by: "),
        div(a(
          "Kyle Grealis",
          href = "https://github.com/kyleGrealis/category5",
          target = "_blank",
          class = "my-links"
        )),
      ),
      span("The Azimuth Project 2023", style = "font-size: 10px"),
      class = "sidebar-signature"
    )
  ), # sidebar



# plots -------------------------------------------------------------------

  # main section
  layout_columns(
    col_widths = c(6, 6),

    # left plot
    card(
      card_header(textOutput("leftCardHeader")),
      card_footer("Displaying your desired sample size and ± 20 participants per group."),
      echarts4rOutput("power"),
    ),

    # right plot
    card(
      card_header(textOutput("rightCardHeader")),
      card_footer("The effect size line displays the necessary sample size and power."),
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

) # ui
