test_that("no errors with typical use", {
  expect_error(
    head(trial) %>%
      gt::gt() %>%
      style_tbl_compact(),
    NA
  )

  expect_error(
    head(trial) %>%
      flextable::flextable() %>%
      style_tbl_compact(),
    NA
  )

  expect_error(
    head(trial) %>%
      huxtable::huxtable() %>%
      style_tbl_compact(),
    NA
  )

  expect_error(
    head(trial) %>%
      knitr::kable(format = "html") %>%
      style_tbl_compact(),
    NA
  )
})
