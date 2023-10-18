test_that("add_inline_forest_plot function works", {
  expect_error(add_inline_forest_plot_ex1 <-
    lm(mpg ~ cyl + am + drat, mtcars) %>%
    gtsummary::tbl_regression() %>%
    add_inline_forest_plot(),
    NA
    )

  # error thrown when class is not gtsummary
  expect_error(add_inline_forest_plot_ex1 <-
                 lm(mpg ~ cyl + am + drat, mtcars) %>%
                 add_inline_forest_plot(),
               "`x=` must be class 'gtsummary'"
  )

  # error thrown when table does not contain estimate, or CI's
  expect_error(add_inline_forest_plot_ex1 <-
                 gtsummary::tbl_summary(mtcars[c("cyl", "am", "drat")]) %>%
                 add_inline_forest_plot(),
               "`x$table_body` must contain columns 'estimate', 'conf.low', 'conf.high'", fixed = TRUE
  )
})
