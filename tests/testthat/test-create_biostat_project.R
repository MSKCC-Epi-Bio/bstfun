test_that("create_bst_project works", {

  expect_error(
    create_bst_project(path = withr::local_tempdir()),
    NA
  )

})

test_that("error if you don't specify a path for project folder", {

  expect_error(
    create_bst_project(path_data = withr::local_tempdir()),
    "*"
  )

})
