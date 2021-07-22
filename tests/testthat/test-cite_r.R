test_that("cite_r works", {
  expect_error(
    cite_r(pkgs = c("tidyverse", "gtsummary")),
    NA
  )

  expect_error(
    cite_r(pkgs = "tidyverse"),
    NA
  )

  expect_error(
    cite_r(pkgs = "tidyverse", add_citations = FALSE),
    NA
  )

  expect_error(
    cite_r(pkgs = NULL),
    NA
  )
})
