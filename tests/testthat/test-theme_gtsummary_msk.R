context("test-theme_gtsummary_msk")

test_that("theme_gtsummary_msk works", {
  expect_error(
    c("hot", "karissa", "ally", "mauguen",
      "esther", "curry", "lavery", "meier") %>%
      map(~theme_gtsummary_msk(.x)),
    NA
  )

  gtsummary::reset_gtsummary_theme()
})
