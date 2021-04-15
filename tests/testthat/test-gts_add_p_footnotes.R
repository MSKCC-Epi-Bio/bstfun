context("test-gts_add_p_footnotes")

library(gtsummary)
gts_tbl <- trial %>%
  select(trt, age, grade) %>%
  tbl_summary(by = trt) %>%
  add_p()

test_that("gts_add_p_footnotes works", {
  expect_error(gts_add_p_footnotes(gts_tbl), NA)
})
