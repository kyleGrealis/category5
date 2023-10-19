box::use(
  shiny[a, div, em, icon, p, span],
  shiny.fluent[Stack, Text]
)

app_note <- div(p("To achieve at least 80% power, your study will need:"))

# my_footer <- div(
#   class = "footer",
#   span(
#     em("Written & developed by: "),
#     a(
#       "Kyle Grealis",
#       icon("github"),
#       href = "https://github.com/kyleGrealis/category5",
#       target = "_blank",
#       class = "my-links"
#     ),
#   ),
#   div(
#     class = "project",
#     "The Azimuth Project ©2023",
#   )
# )

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(
    variant = "medium",
    em("Written & developed by: Kyle Grealis "),
    a(icon("github"),
      href = "https://github.com/kyleGrealis/category5",
      target = "_blank", style = "color: black;"
    ),
    block=TRUE
  ),
  Text(variant = "medium", nowrap = FALSE, "The Azimuth Project ©2023"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)
