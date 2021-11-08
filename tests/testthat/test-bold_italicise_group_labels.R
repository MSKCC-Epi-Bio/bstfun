library(gtsummary)
library(dplyr)

test_that("bold_italicise_group_labels_gt_works", {
  expect_error(
    bold_italicise_group_labels_ex1 <-
      trial %>%
      select(age, trt, grade) %>%
      tbl_strata(
        strata = grade,
        ~ .x %>%
          select(trt, age) %>%
          tbl_summary(by = trt, missing = "no"),
        .combine_with = "tbl_stack"
      ) %>%
      bold_italicise_group_labels(bold = TRUE, "gt"),
    NA
  )
})

test_that("bold_italicise_group_labels_flextable_works", {
  expect_error(
    bold_italicise_group_labels_ex1 <-
      trial %>%
      select(age, trt, grade) %>%
      tbl_strata(
        strata = grade,
        ~ .x %>%
          select(trt, age) %>%
          tbl_summary(by = trt, missing = "no"),
        .combine_with = "tbl_stack"
      ) %>%
      bold_italicise_group_labels(bold = TRUE, "flextable"),
    NA
  )
})

test_that("bold_italicise_group_labels_huxtable_works", {
  expect_error(
    bold_italicise_group_labels_ex1 <-
      trial %>%
      select(age, trt, grade) %>%
      tbl_strata(
        strata = grade,
        ~ .x %>%
          select(trt, age) %>%
          tbl_summary(by = trt, missing = "no"),
        .combine_with = "tbl_stack"
      ) %>%
      bold_italicise_group_labels(bold = TRUE, "huxtable"),
    NA
  )
})

