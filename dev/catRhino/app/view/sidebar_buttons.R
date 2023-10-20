box::use(
  shiny.blueprint[Button, ButtonGroup, Divider, triggerEvent]
)

#' @export
extra_buttons <- list(
  ButtonGroup(
    class = "sidebar-buttons",
    minimal = TRUE,
    Button(
      onClick = triggerEvent("reset"),
      icon = "refresh",
      "Reset"
    ),
    Divider(),
    Button(
      onClick = triggerEvent("help_me"),
      icon = "lightbulb",
      "Help!"
    )
  )
)
