test_that("use_varnames_as_labels works", {
  expect_error(
    mtcars_labeled <-
      mtcars %>%
      use_varnames_as_labels(caps = c(hp, mpg, am, vs), exclude = cyl),
    NA
  )

  # check the labels applied correctly
  expect_equal(
    mtcars_labeled %>% purrr::map(~attr(.x, "label")) %>%
      purrr::compact() %>% unlist() %>% unname(),
    c("MPG", "Disp", "HP", "Drat", "Wt", "Qsec", "VS", "AM", "Gear", "Carb")
  )

  # check the order of the columns did not change
  expect_equal(
    names(mtcars_labeled),
    names(mtcars)
  )
})
