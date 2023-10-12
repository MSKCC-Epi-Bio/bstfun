set.seed(12345)
df <-
  tibble::tibble(
    f1 =
      sample.int(100, n = 3, replace = TRUE) %>%
      factor(levels = 1:3, labels = c("bad", "meh", "good")),
    f2 =
      sample.int(100, n = 3, replace = TRUE) %>%
      factor(levels = 1:3, labels = c("bad", "meh", "good")),
  )

test_that("data for tbl_likert() works", {
  expect_equal(
    str(df),
    df %>%
      mutate(
        dplyr::across(
          .cols = everything(),
          function(.x){
            if (inherits(.x, "factor")) return(.x)
            factor(.x)
          }
        )
      ) %>% str()
  )
})

test_that("tbl_likert() works", {
  expect_error(
    tbl <- tbl_likert(df, digits = ~c(0, 0, 1), statistic = "{n} / {N} ({p}%)"),
    NA
  )
  expect_equal(
    gtsummary::as_tibble(tbl, col_labels = FALSE),
    tibble::tribble(
      ~label,            ~stat_1,            ~stat_2,            ~stat_3,
      "f1",   "27 / 100 (27.0%)", "35 / 100 (35.0%)", "38 / 100 (38.0%)",
      "f2",   "29 / 100 (29.0%)", "35 / 100 (35.0%)", "36 / 100 (36.0%)"
    )
  )

  expect_equal(
    tbl_likert(df, sort = "ascending") %>%
      gtsummary::as_tibble(col_labels = FALSE) %>%
      dplyr::pull(label),
    c("f2", "f1")
  )


})

test_that("add_n.tbl_likert() works", {
  expect_error(
    tbl <- tbl_likert(df),
    NA
  )

  expect_error(
    tbl2 <- tbl %>% add_n(statistic = "{n_miss} {n}", col_label = "testing"),
    NA
  )
  expect_equal(
    gtsummary::as_tibble(tbl2, col_labels = FALSE) %>% purrr::pluck("n"),
    c("0 100", "0 100")
  )
  expect_equal(
    gtsummary::as_tibble(tbl2) %>% names() %>% purrr::pluck(2),
    "testing"
  )

  expect_equal(
    tbl %>% add_n(last = TRUE) %>% gtsummary::as_tibble() %>% dplyr::pull(5),
    c("100", "100")
  )


})


test_that("add_continuous_stat.tbl_likert() works", {
  expect_error(
    tbl <- tbl_likert(df),
    NA
  )

  expect_equal(
    tbl %>%
      add_continuous_stat(
        statistic = "{mean} ({median})",
        digits = ~3,
        score_values = 101:103
      ) %>%
      gtsummary::as_tibble(col_labels = FALSE) %>%
      purrr::pluck("add_stat_1"),
    c("102.110 (102.000)", "102.070 (102.000)")
  )

  expect_equal(
    tbl %>%
      add_continuous_stat(last = FALSE, col_label = "testing") %>%
      gtsummary::as_tibble(col_labels = TRUE) %>%
      names() %>%
      purrr::pluck(2),
    "testing"
  )


})
