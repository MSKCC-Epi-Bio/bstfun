test_that("no errors with typical use", {
  expect_error(
    head(trial) %>%
      gt::gt() %>%
      compact_table(),
    NA
  )

  expect_error(
    head(trial) %>%
      flextable::flextable() %>%
      compact_table(),
    NA
  )

  expect_error(
    head(trial) %>%
      huxtable::huxtable() %>%
      compact_table(),
    NA
  )

  expect_error(
    head(trial) %>%
      knitr::kable(format = "html") %>%
      compact_table(),
    NA
  )
})
