box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu,
        nav_panel, layout_sidebar],
  shiny[div, icon, markdown, moduleServer, NS],
)

box::use(
  app/view/t_test/t_test,
  app/view/uneq_t_test/uneq_t_test,
  app/view/one_samp_prop/one_samp_prop,
  app/view/two_samp_prop/two_samp_prop,
  app/view/uneq_samp_prop/uneq_samp_prop,
  app/view/anova/anova,
  app/view/corr/corr,
  app/view/chisq/chisq,
  app/view/glm/glm,
  
  app/view/footer,
  
  app/logic/helpMe
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = "PowerViz: Power Calculation and Visualization Tool",
    underline = TRUE,
    div(
      style = "margin: 0 auto; max-width: 1300px;",
      navset_underline(
        nav_spacer(),
        nav_menu(
          title = "t-test",
          align = "right",
          t_test$ui(ns("t_test")),
          uneq_t_test$ui(ns("uneq_t_test"))
        ),
        nav_menu(
          title = "Proportions",
          align = "right",
          one_samp_prop$ui(ns("one_samp_prop")),
          two_samp_prop$ui(ns("two_samp_prop")),
          uneq_samp_prop$ui(ns("uneq_samp_prop")),
        ),
        anova$ui(ns("anova")),
        corr$ui(ns("corr")),
        chisq$ui(ns("chisq")),
        glm$ui(ns("glm")),
        nav_panel(
          class = "markdown-panel",
          shiny::icon("circle-info"), 
          helpMe$helpMe
        )
      )
    ),
    div(footer$footer)
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    t_test$server("t_test")
    uneq_t_test$server("uneq_t_test")
    one_samp_prop$server("one_samp_prop")
    two_samp_prop$server("two_samp_prop")
    uneq_samp_prop$server("uneq_samp_prop")
    anova$server("anova")
    corr$server("corr")
    chisq$server("chisq")
    glm$server("glm")
    
  })
}
