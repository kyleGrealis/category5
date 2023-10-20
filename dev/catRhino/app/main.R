box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu,
        nav_panel, layout_sidebar],
  shiny[div, icon, markdown, moduleServer, NS, withMathJax],
)

box::use(
  app/view/t_test,
  app/view/uneq_t_test,
  app/view/one_samp_prop,
  app/view/two_samp_prop,
  app/view/uneq_samp_prop,
  app/view/anova,
  app/view/corr,
  app/view/chi_sq,
  app/view/glm,

  app/logic/text
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = "CAT 5: A Power Calculator & Plotting Tool",
    underline = TRUE,
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
      chi_sq$ui(ns("chi_sq")),
      glm$ui(ns("glm")),
      nav_panel(shiny::icon("circle-info"), shiny::markdown("{pwr} link"))
    ),
    div(class = "footer", text$footer)
  ) # page_navbar
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    t_test$server("t_test")
    uneq_t_test$server("uneq_t_test")
    one_samp_prop$server("one_samp_prop")
    anova$server("anova")
  })
}
