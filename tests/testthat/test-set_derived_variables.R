test_that("set_derived_variables() works", {

  expect_error(
    set_derived_variables(gtsummary::trial, test_path("derived_variables_test.xlsx")),
    NA
  )

  expect_true(
    set_derived_variables(gtsummary::trial, test_path("derived_variables_test.xlsx")) %>%
      pull(age) %>% attr("label") ==
    readxl::read_excel(test_path("derived_variables_test.xlsx"), sheet = "df_main") %>%
      filter(varname == "age") %>% pull(label)
  )

})

test_that("error if file or sheet does not exist", {

  expect_error(
    set_derived_variables(gtsummary::trial, test_path("derived_variables.xlsx")),
    "*"
  )

  expect_error(
    set_derived_variables(gtsummary::trial, test_path("derived_variables_test.xlsx"), sheet = "wrong"),
    "*"
  )

})

test_that("error if correct column names don't exist", {

 expect_error(
   set_derived_variables(gtsummary::trial, test_path("derived_variables_test.xlsx"), sheet = "df_main_wrongvar"),
   "*"
 )

})

test_that("drop option gives correct variable list", {

  expect_named(
    set_derived_variables(gtsummary::trial, test_path("derived_variables_test.xlsx"), sheet = "df_main", drop = TRUE),
    readxl::read_excel(test_path("derived_variables_test.xlsx"), sheet = "df_main") %>% pull(varname),
    ignore.order = TRUE
  )

  expect_named(
    set_derived_variables(gtsummary::trial, test_path("derived_variables_test.xlsx"), sheet = "df_main", drop = FALSE),
    gtsummary::trial %>% names(),
    ignore.order = TRUE
  )

})
