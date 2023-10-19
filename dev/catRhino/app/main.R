box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu, nav_panel,
        layout_sidebar],
  shiny[moduleServer, NS, withMathJax],
)

box::use(
  app/view/t_test,
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
        nav_panel(
          "Unequal sample sizes",
          layout_sidebar(
            sidebar = "uneq-ttest",
            "Unequal sample sizes t-test here soon"
          )
        )
      )
    ),
    text$my_footer
  ) # page_navbar
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    t_test$server("t_test")
  })
}
