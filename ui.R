
ui <- page_sidebar(
  tags$head(
    tags$style(sass(sass_file('www/styles.scss')))
  ),
  title = div(
    "CAT 5: A Hurricane's Power Calculator & Plotting Tool",
    class = 'my-title'
  ),
  sidebar = sidebar(
    title = "Plotting controls",
    selectInput('test', 
                'For which statistical test would you like to calculate power?',
                choices = c('t-test', 'More coming soon!'),
                selected = 't-test'),
    selectInput("alpha", "Choose your significance level",
                choices = c(0.01, 0.025, 0.05),
                selected = 0.05),
    
    numericInput("effect", "Choose the desired effect size",
                 min = 0.1, max = 1.0, step = 0.05, value = 0.5),
    
    numericInput('sample', 'Choose the sample size per group',
                 min = 20, max = 300, step = 5, value = 100)
  ), # sidebar
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header(h3('Selected sample size')),
      card_footer('Displaying your desired sample size and ± 20 participants per group.'),
      echarts4rOutput('power'),
      min_height = 200, max_height = 600,
    ),
    card(
      card_header(h3('Selected effect size')),
      card_footer('The effect size line displays the necessary sample size and power.'),
      echarts4rOutput('power2'),
      min_height = 200, max_height = 600
    ),
    style='max-height: 39rem;'
  ), # layout_columns
  p('To achieve at least 80% power, your study will need:'),
  layout_columns(
    # left box
    div( # flip-box
      div( # flip-box-inner
        div( # flip-box-front
          value_box(
            title = 'Overall measurable effect size',
            value = textOutput('effectSize'),
            showcase = bsicons::bs_icon("thermometer-low"),
            class = 'left-box'
          ),
          class = 'flip-box-front'
        ),
        div(
          value_box(
            title = 'backside test',
            value = textOutput('backsideTest'),
            showcase = bsicons::bs_icon("thermometer-low"),
            class = 'left-box'
          ),
          class = 'flip-box-back'
        ),
        class = 'flip-box-inner'
      ),
      class = 'flip-box'
    ),
    
    # center box
    value_box(
      title = 'Minimal sample size per group',
      value = textOutput('minSampleSize'),
      showcase = bsicons::bs_icon("speedometer2"),
      theme_color = 'white'
    ),
    
    # right box
    value_box(
      title = 'Your proposed study power will be:',
      value = textOutput('other'),
      showcase = bsicons::bs_icon("thermometer-high"),
      class = 'right-box'
    ),
    style = 'max-height: 200px;'
  ), # layout_columns
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
  ),
  # style='background-image: linear-gradient(to bottom right, #f47321, #005030);'
) # ui