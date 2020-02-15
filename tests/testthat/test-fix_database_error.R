
df_master_test <-
  tibble::tribble(
    ~mrn, ~surgdate, ~age, ~psa, ~stage, ~bcr, ~bcr_posix, ~bcr_date,
    "0001", lubridate::ymd("2010-01-01"), 65L, 4.4, "T1", TRUE, as.Date("2011-01-01"), lubridate::ymd("2011-01-01"),
    "0002", lubridate::ymd("2011-06-01"), 80L, 10.2, "T2", FALSE, NA, NA,
    "0003", lubridate::ymd("2013-09-15"), 57L, 7.3, "T2", FALSE, NA, NA,
    "0004", lubridate::ymd("2017-12-12"), 70L, 23, "T3", TRUE, as.Date("2019-01-01"), lubridate::ymd("2019-01-01"),
    "0005", lubridate::ymd("2018-01-01"), 52L, 3.1, "T1", FALSE, NA, NA
  ) %>%
  dplyr::mutate(ttbcr = difftime(bcr_posix, surgdate))

df_fixes_test <-
  tibble::tribble(
    ~id, ~variable, ~value,
    'mrn=="0001"', "age", "70",
    'mrn=="0002" & surgdate == lubridate::ymd("2011-06-01")', "psa", "11.2",
    'surgdate == lubridate::ymd("2013-09-15")', "stage", "T4",
    'mrn=="0003"', "surgdate", "2013-09-30",
    'mrn=="0005"', "bcr", "TRUE",
    'mrn=="0005"', "bcr_posix", "2019-06-01",
    'mrn=="0005"', "bcr_date", "2019-06-01",
    'mrn=="0005"', "ttbcr", "516"
  )

test_that("fix_database_error runs without error", {

  expect_error(
    fix_database_error(df_master_test, engine = I, x = df_fixes_test),
    NA
  )
})

test_that("fix_database_error catches wrong column names for either file", {

  expect_error(
    fix_database_error(df_master_test, engine = I,
                       x = df_fixes_test %>% dplyr::rename(column_name = variable)),
    "*"
  )
  expect_error(
    fix_database_error(df_master_test, engine = I,
                       x = df_fixes_test %>%
                         dplyr::mutate(variable = dplyr::if_else(variable == "age", "age2", variable))),
    "*"
  )
})

test_that("fix_database_error does not allow duplicates in either file", {

  expect_error(
    fix_database_error(df_master_test, engine = I,
                       x = df_fixes_test %>%
                         dplyr::mutate(freq = 2) %>%
                         tidyr::uncount(freq)),
    "*"
  )
  expect_error(
    fix_database_error(df_master_test %>% dplyr::mutate(freq = 2) %>% tidyr::uncount(freq),
                       engine = I,
                       x = df_fixes_test),
    "*"
  )
})

test_that("fix_database_error does not allow factors", {

  expect_error(
    fix_database_error(df_master_test %>% dplyr::mutate(stage = factor(stage), bcr = factor(bcr)),
                       engine = I,
                       x = df_fixes_test),
    "*"
  )
})

test_that("fix_database_error does not allow discrepant data types", {

  expect_error(
    fix_database_error(df_master_test, engine = I,
                       x = df_fixes_test %>%
                         dplyr::mutate(value = dplyr::if_else(variable == "age", "70.5", value))),
    "*"
  )
  expect_error(
    fix_database_error(df_master_test, engine = I,
                       x = df_fixes_test %>%
                         dplyr::mutate(value = dplyr::if_else(value == "2019-06-01", "06-01-2019", value))),
    "*"
  )
})

test_that("fix_database_error does not allow non-logical expressions in `id`", {

  expect_error(
    fix_database_error(df_master_test, engine = I,
                       x = df_fixes_test %>% dplyr::mutate(id = 'mrn = "0001"')),
    "*"
  )
})


