
ui <- page_sidebar(
  
  # include SCSS file
  tags$head(
    tags$style(sass(sass_file('www/styles.scss')))
  ),
  
  # welcome popup modal
  modalDialog(
    h5("This statistical power calculator and plotting tool is
       100% interactive!"),
    br(),
    h5("Hover over the plots to see more information about power,
       effect size, and sample size."),
    br(),
    h5("Hold your pointer on the lower boxes to flip for more information."),
    title = "CAT 5: A Hurricane's Power Calculator & Plotting Tool",
    size = "l",
    easyClose = FALSE,
  ),
  
  # application title
  title = "CAT 5: A Hurricane's Power Calculator & Plotting Tool",
  
  # sidebar / input controls
  sidebar = sidebar(
    title = "Plotting controls",
    selectInput(
      'test', 
      'For which statistical test would you like to calculate power?',
      choices = c('t-test', 'More coming soon!'),
      selected = 't-test'
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
      'sample', 'Choose the sample size per group',
      min = 20, max = 300, step = 5, value = 100
    )
  ), # sidebar
  
  # main section
  layout_columns(
    col_widths = c(6, 6),
    
    # left plot
    card(
      card_header(h3('Selected sample size'), style = "background-color: #d3d3d3;"),
      card_footer('Displaying your desired sample size and ± 20 participants per group.'),
      echarts4rOutput('power')
    ),
    
    # right plot
    card(
      card_header(h3('Selected effect size'), style = "background-color: #d3d3d3"),
      card_footer('The effect size line displays the necessary sample size and power.'),
      echarts4rOutput('power2')
    ),
    style='max-height: 39rem;'
    
  ), # layout_columns
  
  # simple dialogue between plots and flip cards
  p('To achieve at least 80% power, your study will need:'),
  
  # lower display section with flip cards
  layout_columns( 
    
    # left box
    div( # flip-box
      div( # flip-box-inner
        div( # flip-box-front
          value_box(
            title = 'Overall measurable effect size',
            value = textOutput('effectSize'),
            showcase = bsicons::bs_icon("thermometer-low"),
            theme = 'white',
            class = 'left-box'
          ),
          class = 'flip-box-front'
        ),
        # back side of left card
        div(
          value_box(
            title = 'About effect size:',
            value = textOutput('backsideLeft'),
            showcase = bsicons::bs_icon("thermometer-low"),
            theme = 'white',
            class = 'left-box'
          ),
          class = 'flip-box-back'
        ),
        class = 'flip-box-inner'
      ),
      class = 'flip-box'
    ),
    
    # center box
    div( # flip-box
      div( # flip-box-inner
        div( # flip-box-front
          value_box(
            title = 'Minimal sample size per group',
            value = textOutput('minSampleSize'),
            showcase = bsicons::bs_icon("people-fill"),
            theme = 'black',
            class = 'center-box'
          ),
          class = 'flip-box-front'
        ),
        # back side of left card
        div(
          value_box(
            title = 'Sample Size info...',
            value = textOutput('backsideCenter'),
            showcase = bsicons::bs_icon("people-fill"),
            theme = 'black',
            class = 'center-box'
          ),
          class = 'flip-box-back'
        ),
        class = 'flip-box-inner'
      ),
      class = 'flip-box'
    ),
    
    # right box
    div( # flip-box
      div( # flip-box-inner
        div( # flip-box-front
          value_box(
            title = 'Your proposed study power will be:',
            value = textOutput('sampleResult'),
            showcase = bsicons::bs_icon("bullseye"),
            theme = 'black',
            class = 'right-box'
          ),
          class = 'flip-box-front'
        ),
        # back side of left card
        div(
          value_box(
            title = 'Power is calculated by:',
            value = textOutput('backsideRight'),
            showcase = bsicons::bs_icon("bullseye"),
            theme = 'black',
            class = 'right-box'
          ),
          class = 'flip-box-back'
        ),
        class = 'flip-box-inner'
      ),
      class = 'flip-box'
    ),
    style = 'max-height: 200px;'
  ), # layout_columns
  
  # footer
  span(
    p(
      em('Written & developed by: '),
      a(
        'Kyle Grealis', href = 'https://github.com/kyleGrealis/', 
        target = '_blank',
        style = 'color:black;'
      ),
      style='text-align:right;'
    )
  )
) # ui

