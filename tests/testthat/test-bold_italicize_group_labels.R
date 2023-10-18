library(gtsummary)
library(dplyr)

test_that("bold_italicize_group_labels_gt_works", {
  expect_error(
    bold_italicize_group_labels_ex1 <-
      trial %>%
      select(age, trt, grade) %>%
      gtsummary::tbl_strata(
        strata = grade,
        ~ .x %>%
          select(trt, age) %>%
          gtsummary::tbl_summary(by = trt, missing = "no"),
        .combine_with = "tbl_stack"
      ) %>%
      bold_italicize_group_labels(bold = TRUE, italics = TRUE, "gt"),
    NA
  )
})

test_that("bold_italicize_group_labels_flextable_works", {
  expect_error(
    bold_italicize_group_labels_ex1 <-
      trial %>%
      select(age, trt, grade) %>%
      gtsummary::tbl_strata(
        strata = grade,
        ~ .x %>%
          select(trt, age) %>%
          gtsummary::tbl_summary(by = trt, missing = "no"),
        .combine_with = "tbl_stack"
      ) %>%
      bold_italicize_group_labels(bold = TRUE, italics = TRUE, "flextable"),
    NA
  )
})

test_that("bold_italicize_group_labels_huxtable_works", {
  expect_error(
    bold_italicize_group_labels_ex1 <-
      trial %>%
      select(age, trt, grade) %>%
      gtsummary::tbl_strata(
        strata = grade,
        ~ .x %>%
          select(trt, age) %>%
          gtsummary::tbl_summary(by = trt, missing = "no"),
        .combine_with = "tbl_stack"
      ) %>%
      bold_italicize_group_labels(bold = TRUE, italics = TRUE, "huxtable"),
    NA
  )
})

test_that("bold_italicize_group_labels overall function works", {
  expect_error(
      trial %>%
      select(age, trt, grade) %>%
      bold_italicize_group_labels(bold = TRUE),
    "Class of 'x' must be 'gtsummary'"
  )
})
