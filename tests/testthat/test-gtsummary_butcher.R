test_that("gtsummary_butcher works", {
  expect_error(
    tbl <-
      gtsummary::tbl_uvregression(
        trial,
        y = age,
        method = lm,
        include = c(marker, grade)
      ),
    NA
  )

  expect_error(
    tbl_small <- gtsummary_butcher(tbl),
    NA
  )

  expect_true(
    object.size(tbl) > object.size(tbl_small)
  )

})
