test_that("followup_time() works", {
  expect_equal(
    followup_time(survival::Surv(ttdeath, death), data = trial),
    "24 (IQR 24, 24)"
  )

  expect_error(
    followup_time(letters, data = trial)
  )

  expect_error(
    followup_time(survival::Surv(ttde5555ath, death), data = trial)
  )
})
