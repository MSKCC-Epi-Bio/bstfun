test_that("No errors/warnings with standard use", {
  expect_error(
    auc_density(density = dbeta, shape1 = 1, shape2 = 1),
    NA
  )

  expect_error(
    runif(10000) %>% hist(breaks = 250) %>% auc_histogram(),
    NA
  )

### ensuring auc_histogram throws correct error w/ non hist object
  expect_error(
    runif(10000) %>%
      barplot() %>%
      auc_histogram(),
    "`x=` must be class 'histogram' created with `hist()`", fixed = TRUE
  )
})
