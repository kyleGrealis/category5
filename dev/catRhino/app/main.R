box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu, nav_panel,
        layout_sidebar],
  shiny[div, moduleServer, NS, withMathJax],
)

box::use(
  app/view/t_test,
  app/view/uneq_t_test,
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
      # anova$ui(ns("anova")),
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
      nav_panel("GLM", "General linear model results here")
    ),
    div(class = "footer", text$footer)
  ) # page_navbar
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    t_test$server("t_test")
    uneq_t_test$server("uneq_t_test")
    # anova$server("anova")
  })
}
