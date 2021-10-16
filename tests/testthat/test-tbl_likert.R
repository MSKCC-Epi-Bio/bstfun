df <-
  tibble::tibble(
    f1 =
      sample.int(100, n = 3, replace = TRUE) %>%
      factor(levels = 1:3, labels = c("bad", "meh", "good")),
    f2 =
      sample.int(100, n = 3, replace = TRUE) %>%
      factor(levels = 1:3, labels = c("bad", "meh", "good")),
  )


test_that("tbl_likert() works", {
  expect_error(
    tbl_likert(df) %>%
      add_n(),
    NA
  )
})
