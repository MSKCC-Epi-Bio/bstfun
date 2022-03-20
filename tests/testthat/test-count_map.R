test_that("No Errors/Warning with standard use", {
  expect_error(
    trial %>% count_map(c(stage, grade), c(grade, response)),
    NA
  )
  expect_warning(
    trial %>% count_map(c(stage, grade), c(grade, response)),
    NA
  )
})

test_that("Works for a single vector", {
  expect_error(
    trial %>% count_map(c(stage, grade)),
    NA
  )
})
