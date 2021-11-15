library(gtsummary)
library(tidyverse)
library(labelled)

# Import Data -----------------------------------------------------

raw_file_name <- ""

df_raw_data <-
  readxl::read_excel(
    path = here::here("raw-data", raw_file_name)
  ) %>%
  mutate(across(where(lubridate::is.POSIXt),
                lubridate::as_date))



# Clean Data -----------------------------------------------------


# label data
df_main <- df_main %>%
  set_variable_labels(.labels = kwiktools::names_to_labels(.))

# Save Data -----------------------------------------------------
# saving master analytic data set
kwiktools::save_date(
    object = df_main,
    path = here::here("data", "df_main.Rds"))

# Data Checks  -----------------------------------------------------
