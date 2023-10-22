box::use(
  shiny[a, em, icon],
  shiny.fluent[Stack, Text]
)

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
  # Text(variant = "medium", nowrap = FALSE, "The Azimuth Project ©2023"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved ©2023")
)
