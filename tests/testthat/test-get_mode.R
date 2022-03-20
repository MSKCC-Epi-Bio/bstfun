context("get_mode")

library(gtsummary)

test_that("get_mode function works on numeric, character or factor", {
  expect_error(
    get_mode(trial$age),
    NA
  )

  expect_error(
    get_mode(trial$trt),
    NA
  )

  expect_error(
    get_mode(trial$stage),
    NA
  )
})

test_that("Message printed if multiple modes and `moden` not specified", {
  expect_message(
    get_mode(trial$age),
    "*"
  )
})

test_that("Message not printed if multiple modes and `moden` specified", {
  expect_message(
    get_mode(trial$age, moden = 2),
    NA
  )
})

test_that("Message not printed if `quiet` is `TRUE", {
  expect_message(
    get_mode(trial$age, quiet = TRUE),
    NA
  )
})
