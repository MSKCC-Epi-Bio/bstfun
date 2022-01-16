test_that("tbl_checkbox() works", {
  set.seed(11234)
  expect_error(
    tbl_checkbox_ex1 <-
      data.frame(
        race_asian = sample(c(TRUE, FALSE), 20, replace = TRUE),
        race_black = sample(c(TRUE, FALSE), 20, replace = TRUE),
        race_white = sample(c(TRUE, FALSE), 20, replace = TRUE),
        age = rnorm(20, mean = 50, sd = 10)
      ) %>%
      tbl_checkbox(
        include =
          list("Race (check all that apply)" = c("race_asian", "race_black", "race_white"),
               "age"),
        label = list(race_asian = "Asian",
                     race_black = "Black",
                     race_white = "White",
                     age = "Age")
      ),
    NA
  )
})
