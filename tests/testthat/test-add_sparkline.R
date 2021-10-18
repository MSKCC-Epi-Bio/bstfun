test_that("add_sparkline() works", {
  tbl <-
    trial %>%
    select(age, marker) %>%
    gtsummary::tbl_summary(missing = "always")

  expect_error(
    tbl %>% add_sparkline(type = "histogram"),
    NA
  )
  expect_error(
    tbl %>% add_sparkline(type = "sparkline"),
    NA
  )

  expect_error(
    tbl %>% add_sparkline(column_header = letters)
  )

})
