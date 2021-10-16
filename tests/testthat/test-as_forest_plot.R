library(gtsummary)
library(survival)

test_that("as_forest_plot() works", {
  expect_error(
    tbl_uvregression(
      trial[c("response", "age", "grade")],
      method = glm,
      y = response,
      method.args = list(family = binomial),
      exponentiate = TRUE
    ) %>%
      as_forest_plot(),
    NA
  )
})
