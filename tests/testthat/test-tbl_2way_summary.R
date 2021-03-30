test_that("tbl_2way_summary()", {
  expect_error(
    trial %>%
      tbl_2way_summary(grade, trt, marker) %>%
      gtsummary::as_gt(),
    NA
  )
})
