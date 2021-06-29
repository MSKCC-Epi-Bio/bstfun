library(biostatR)
library(tidyverse)


# example code for importing excel file
df_raw_data <-
  readxl::read_excel(
    path = here_data("Raw Data from PI.xlsx")
  ) %>%
  mutate(across(where(lubridate::is.POSIXt),
                lubridate::as_date))




# overview of master analytic dataset
skimr::skim(df_main)

# saving master analytic data set
kwiktools::save_date(
    object = df_main,
    path = here::here("data", "df_main.Rds"))

