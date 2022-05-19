test_that("add_sparkline() works", {
  tbl <-
    trial %>%
    dplyr::select(age, marker) %>%
    gtsummary::tbl_summary(missing = "always")

  expect_error(
    purrr::map(
      c("boxplot", "histogram", "rug_strip", "density", "sparkline"),
      ~ add_sparkline(tbl, type = .)
    ),
    NA
  )

  expect_error(
    tbl %>% add_sparkline(column_header = letters)
  )

})
