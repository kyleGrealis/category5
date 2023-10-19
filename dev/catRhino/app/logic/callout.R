box::use(
  shiny[p],
  shiny.blueprint[Callout]
)

ttest <- Callout(
  title = "This is a test for differences in means.",
  p("Two sample: Is there a difference in the mean income of men and women?"),
  p("One sample: Is the supposed (null) value different than the true value?"),
  p("Paired: Does an exercise program increase speed in the 100m sprint?
      Are the times different (time1 - time2)?")
)
