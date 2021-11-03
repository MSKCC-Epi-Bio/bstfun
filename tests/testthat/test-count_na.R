context("count_na")

test_that("No Errors/Warning with standard use", {
  expect_error(
    trial %>% count_na(),
    NA
  )
  expect_error(
    trial %>% count_na(include = c("marker", "stage")),
    NA
  )

  expect_warning(
    trial %>% count_na(),
    NA
  )
  expect_warning(
    trial %>% count_na(include = c("marker", "stage")),
    NA
  )
})
