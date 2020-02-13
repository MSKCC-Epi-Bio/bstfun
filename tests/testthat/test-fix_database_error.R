# Create dataset to fix
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

test <- df_fixes_test[8, ]

browser(fix_database_error(df_master_test, engine = I, x = test))



# TODO: Error checks
# Create fixes file with wrong column names
# Create fixes file that includes column names not in original data
# Create fixes files with duplicates
# Create fixes with ID that doesn't evaluate to a logical
# Create fixes with date in wrong format
# Create fixes with integer variable fix as non-integer

# Create main data file with factor
# Create main data file with duplicates
