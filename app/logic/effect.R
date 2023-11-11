box::use(
  dplyr[tibble]
)

# create a grid off possible effect sizes for each statistical test
#' @export
effect_table <- dplyr::tibble(
  size = c("small", "medium", "large"),
  t_test = c(0.2, 0.5, 0.8),
  prop = c(0.2, 0.5, 0.8),
  anova = c(0.1, 0.25, 0.4),
  corr = c(0.1, 0.3, 0.5),
  chisq = c(0.1, 0.3, 0.5),
  glm = c(0.02, 0.15, 0.35)
)
