
ui <- page_navbar(

  # include SCSS file
  tags$head(
    tags$style(sass(sass_file("www/test-styles.scss")))
  ),


# welcome modal -----------------------------------------------------------
  
  modalDialog(
    h5("This statistical power calculator and plotting tool is
       100% interactive!"),
    br(),
    h5("Dynamically change the plots and view the impact on study power when
       changing significance, effect size, and sample size. Click download
       icon inside the plot output to save the image."),
    br(),
    h5("Hover your pointer to flip the lower info cards for package links
       and study design resources."),
    title = "CAT 5: A Power Calculator & Plotting Tool",
    size = "l",
    easyClose = FALSE,
  ),
  
  
  

# bslib page_navbar options -----------------------------------------------

# title -------------------------------------------------------------------

  title = "CAT 5: A Power Calculator & Plotting Tool",
  underline = TRUE,
  navset_underline(
    nav_spacer(),
    nav_menu(
      title = "t-test",
      align = "right",
      nav_panel(
        "Means", 
        layout_sidebar(
          sidebar = my_sidebar, 
          display_here
        ),
        
      ),
      nav_panel("Unequal sample sizes", "Unequal sample sizes t-test here soon")
    ),
    nav_menu(
      title = "Proportions",
      align = "right",
      nav_panel("One sample", "this one will work"),
      nav_panel("Two sample", "this one will NEED work"),
      nav_panel("Unequal n samples", "this one will NEED work also")
    ),
    nav_panel("ANOVA", "ANOVA results here"),
    nav_panel("Correlation", "Correlation results here"),
    nav_panel(withMathJax("$$X^2$$"), "chi square results here"),
    nav_panel("GLM", "General linear model results here"),
  ),
  

  # sidebar = my_sidebar,


# footer ------------------------------------------------------------------

  div(
    class = "footer",
    span(
      em("Written & developed by: "),
      a(
        "Kyle Grealis",
        shiny::icon("github"),
        href = "https://github.com/kyleGrealis/category5",
        target = "_blank",
        class = "my-links"
      ),
    ),
    div(
      class = "project",
      "The Azimuth Project ©2023", 
    )
  )


) # ui
