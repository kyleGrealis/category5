box::use(
  shiny[div, span, em, a, icon]
)

#' @export
my_footer <- div(
  class = "footer",
  span(
    em("Written & developed by: "),
    a(
      "Kyle Grealis",
      icon("github"),
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

