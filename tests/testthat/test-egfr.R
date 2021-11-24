test_that("egfr_*() works", {
  expect_error(
    egfr_mdrd(creatinine = 1.2, age = 60, female = TRUE, aa = TRUE),
    NA
  )
  expect_error(
    egfr_ckdepi(creatinine = 1.2, age = 60, female = TRUE, aa = TRUE),
    NA
  )

  expect_error(
    egfr_mdrd(creatinine = 1.2, age = 60, female = TRUE, aa = 1)
  )
  expect_error(
    egfr_ckdepi(creatinine = 1.2, age = 60, female = TRUE, aa = 1)
  )
})
