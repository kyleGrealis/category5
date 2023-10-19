box::use(
  shiny[a, div, em, icon, p, span]
)

app_note <- div(p("To achieve at least 80% power, your study will need:"))

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
