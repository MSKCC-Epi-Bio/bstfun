test_that("followup_time() works", {
  expect_equal(
    followup_time(survival::Surv(ttdeath, death), data = trial),
    "24 (IQR 24, 24)"
  )

  expect_equal(
    followup_time(
      survival::Surv(ttdeath, death),
      data = trial,
      pattern = "{n} events with a follow-up time of {median} months (IQR {p25}, {p75})",
      style_fun = function(x) gtsummary::style_sigfig(x, digits = 4)
    ),
    "112 events with a follow-up time of 24.00 months (IQR 24.00, 24.00)"
  )

  expect_error(
    followup_time(letters, data = trial)
  )

  expect_error(
    followup_time(survival::Surv(ttde5555ath, death), data = trial)
  )
})
