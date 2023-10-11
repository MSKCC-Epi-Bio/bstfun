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

### ensuring "Error: `x=` must be class 'tbl_regression' or 'tbl_uvregression'"
  expect_error((
    tbl_summary(
      trial[c("response", "age", "grade")],
      by = "response")
  ) %>%
    as_forest_plot(),
  "`x=` must be class 'tbl_regression' or 'tbl_uvregression'"
  )
})



