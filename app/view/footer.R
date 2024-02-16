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
      href = "https://github.com/kyleGrealis/case-control-matching",
      target = "_blank", style = "color: black;"
    ),
    block=TRUE
  ),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved ©2024"),
  style = "position: fixed; width: 100%; left: 0; bottom: 0; color: black; padding: 2px 20px; border-top: 0.4px solid black; background-color: #f3f3f3;"
)
