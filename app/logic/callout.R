box::use(
  shiny[div, p],
  shiny.blueprint[Callout]
)

Callout <- function(...) div(class = "callout-block", ...)

app_note <- Callout(
  class = "app-note",
  "To achieve at least 80% power, your study will need:"
)

ttest <- Callout(
  title = "This is a test for differences in means.",
  p("Two sample: Is there a difference in the mean income of men and women?"),
  p("One sample: Is the supposed (null) value different than the true value?"),
  p("Paired: Does an exercise program decrease time in a 5K run? Are the times
  different (time1 - time2)?")
)

uneq_t_test <- Callout(
  title = "This is a test of differences in means with unequal sample sizes.",
  p("Example: Is the mean income in Group 1 (with 15 participants) different
    than the mean income in Group 2 (with 20 participants)?")
)

one_samp_prop <- Callout(
  title = "This is a test of one-sample proportion.",
  p('Example: Is that coin fair? Is the proportion of the outcome
      "heads" different than the expected 50%?')
)

two_samp_prop <- Callout(
  title = "This is a test of two-sample proportions.",
  p("Example: Is the proportion of seatbelt use greater in Group A
      than in Group B?")
)

uneq_samp_prop <- Callout(
  title = "This is a test of two-sample proportions of unequal group sizes.",
  p("Example: Is the proportion of seatbelt use greater in Group A
      than in Group B?")
)

anova <- Callout(
  title = "This is a test of differences in means across k groups.",
  p("Example: Assuming equal sample sizes per group, is the mean weight
    loss from k exercise groups different?")
)

corr <- Callout(
  title = "This is a test of the estimated correlation.",
  p("Example: Is the correlation between the number of hours a student studies
      and final test average significantly different than 0?")
)

chi_sq <- Callout(
  title = "This is a test of goodness of fit group proportions.",
  p("Example: Is the proportion of adherence to exercise habits in Group A
      different from the proportion(s) in all other groups?")
)

glm <- Callout(
  title = "This is a test of regression coefficients.",
  p("Example: Are any regression coefficients statistically
    distinguishable from 0?")
)
