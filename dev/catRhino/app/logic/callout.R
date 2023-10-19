box::use(
  shiny[p],
  shiny.blueprint[Callout]
)

ttest <- Callout(
  title = "This is a test for differences in means.",
  p("Two sample: Is there a difference in the mean income of men and women?"),
  p("One sample: Is the supposed (null) value different than the true value?"),
  p("Paired: Does an exercise program decrease time in a 5K run?
      Are the times different (time1 - time2)?")
)

uneq_ttest <- Callout(
  title = "This is a test of differences in means with unequal sample sizes.",
  p("Example: Is the mean income in Group 1 (with 50 participants) different
    than the mean income in Group 2 (with 75 participants)?")
)

anova <- Callout(
  title = "This tests differences in means across k groups.",
  p("Example: Assuming equal sample sizes per group, is the mean weight
    loss from k exercise groups different?")
)
