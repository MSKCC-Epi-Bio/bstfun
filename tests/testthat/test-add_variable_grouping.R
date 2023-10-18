test_that("add_variable_grouping() works", {
  set.seed(11234)
  expect_error(
    data.frame(
      race_asian = sample(c(TRUE, FALSE), 20, replace = TRUE),
      race_black = sample(c(TRUE, FALSE), 20, replace = TRUE),
      race_white = sample(c(TRUE, FALSE), 20, replace = TRUE),
      age = rnorm(20, mean = 50, sd = 10)
    ) %>%
      gtsummary::tbl_summary(
        label = list(race_asian = "Asian",
                     race_black = "Black",
                     race_white = "White",
                     age = "Age")
      ) %>%
      add_variable_grouping(
        "Race (check all that apply)" = c("race_asian", "race_black", "race_white")),
    NA
  )

  expect_error(
    data.frame(
      race_asian = sample(c(TRUE, FALSE), 20, replace = TRUE),
      race_black = sample(c(TRUE, FALSE), 20, replace = TRUE),
      race_white = sample(c(TRUE, FALSE), 20, replace = TRUE),
      age = rnorm(20, mean = 50, sd = 10)
    ) %>%
      add_variable_grouping(
        "Race (check all that apply)" = c("race_asian", "race_black", "race_white")),
    "`x=` must be class 'gtsummary'."
  )

  expect_error(
    data.frame(
    race_asian = sample(c(TRUE, FALSE), 20, replace = TRUE),
    race_black = sample(c(TRUE, FALSE), 20, replace = TRUE),
    race_white = sample(c(TRUE, FALSE), 20, replace = TRUE),
    age = rnorm(20, mean = 50, sd = 10)
  ) %>%
    gtsummary::tbl_summary(
    ) %>%
    add_variable_grouping("")
  )
})
