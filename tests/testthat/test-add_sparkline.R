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

### expecting "stop("`x=` must be class 'tbl_summary'", call. = FALSE)" works
  tbl_regression_ex <-
    gtsummary::tbl_uvregression(
      trial[c("response", "age", "grade")],
      method = glm,
      y = response,
      method.args = list(family = binomial),
      exponentiate = TRUE
    )

  expect_error(
    add_sparkline(tbl_reg_ex)
  )

### expecting message to be thrown
  tbl_by <-
    trial[c("age", "marker", "response")] %>%
    gtsummary::tbl_summary(
      by = response,
      missing = "always"
    )

  expect_message(
    tbl_by %>% add_sparkline(),
    "Input table is stratified, but sparkline figure is not."
  )
})

