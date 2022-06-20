test_that("ggcalibration() works", {
  expect_error(
    glm(response ~ age + marker + grade, trial, family = binomial) %>%
      broom::augment(type.predict = "response") %>%
      ggcalibration(y = response, x = .fitted),
    NA
  )
})
