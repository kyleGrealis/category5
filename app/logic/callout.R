box::use(
  shiny[br, div, p],
  shiny.blueprint[Callout]
)

Callout <- function(...) div(class = "callout-block", ...)

app_note <- Callout(
  class = "app-note",
  "To achieve at least 80% power, your study will need:"
)

ttest <- Callout(
  p("Two sample: Is there a difference in the mean income of men and women?",
  br(), "One sample: Is the supposed (null) value different than the true value?",
  br(), "Paired: Does an exercise program decrease time in a 5K run? Are the times
  different (time1 - time2)?")
)

uneq_t_test <- Callout(
  p("Example: Is the mean income in Group 1 (with 15 participants) different
    than the mean income in Group 2 (with 20 participants)?")
)

one_samp_prop <- Callout(
  p('Example: Is that coin fair? Is the proportion of the outcome
      "heads" different than the expected 50%?')
)

two_samp_prop <- Callout(
  p("Example: Is the proportion of seatbelt use greater in Group A
      than in Group B?")
)

uneq_samp_prop <- Callout(
  p("Example: Is the proportion of seatbelt use greater in Group A
      than in Group B?")
)

anova <- Callout(
  p("Example: Assuming equal sample sizes per group, is the mean weight
    loss from k exercise groups different?")
)

corr <- Callout(
  p("Example: Is the correlation between the number of hours a student studies
      and final test average significantly different than 0?")
)

chi_sq <- Callout(
  p("Example: Is the proportion of adherence to exercise habits in Group A
      different from the proportion(s) in all other groups?")
)

glm <- Callout(
  p("Example: Are any regression coefficients statistically
    distinguishable from 0?")
)
