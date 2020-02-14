context("test-tbl_ancova")

test_that("tbl_ancova", {
  expect_error(
    tbl_ancova(trial, y = c("age", "marker"), x = "trt"),
    NA
  )
  expect_error(
    t1 <- tbl_ancova(trial, y = c(age, marker), x = trt,
               digits = age ~ c(0, 1),
               label = age ~ "AGE!"),
    NA
  )

  expect_equal(
    "AGE!",
    t1$table_body %>%
      filter(variable == "age") %>%
      pull(label)
  )
})
