test_that("clean_mrn() works", {
  expect_equal(
    1000:1001 %>% clean_mrn(),
    c("00001000", "00001001")
  )

  expect_error(clean_mrn("a"))
  expect_error(clean_mrn(NA))
  expect_error(clean_mrn("aaaaaaaaa"))
  expect_error(clean_mrn(c("12345678", "12345678"), check_unique = TRUE))
})
