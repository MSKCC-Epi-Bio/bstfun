# library(tidyverse)
#
# # Load data and make sure we have variables of all data types...
# data <-
#   readxl::read_excel("~/GitHub/baddata.xlsx") %>%
#   janitor::clean_names("all_caps") %>%
#   # Create EBL as integer
#   mutate_at(vars(EBL), ~as.integer(.)) %>%
#   # Calculate a difftime to use
#   mutate(TTRECUR = DATE_OF_RECURRENCE_STATUS - DATE_OF_RC) %>%
#   # Dates are posix dates - create lubridate dates to check as well
#   mutate(LUB_DATE_OF_RECURRENCE_STATUS = lubridate::as_date(DATE_OF_RECURRENCE_STATUS),
#          LUB_DATE_OF_RC = lubridate::as_date(DATE_OF_RC),
#          LUB_TTRECUR = LUB_DATE_OF_RECURRENCE_STATUS - LUB_DATE_OF_RC) %>%
#   # Lubridate datetime
#   mutate(NEW_DATE = lubridate::ymd_hms("2020-02-13 12:00:00")) %>%
#   # Create gender a factor
#   #mutate(GENDER = factor(GENDER)) %>%
#   # Create recurrence as a logical
#   mutate(RECURRENCE = as.logical(as.numeric(RECURRENCE)))
#
# data
#
# database_fixes <-
#   readxl::read_excel("~/GitHub/fixes1.xlsx") %>%
#   filter(value != "6/11/2020" & value != "43993")

# This will induce errors, use for checking
#database_fixes_error <- readxl::read_excel("~/GitHub/fixes1.xlsx")

fix_database_error <- function(data, engine = readr::read_csv, ...) {

  # importing list of database fixes -------------------------------------------
  database_fixes <- do.call(engine, list(...))

  # checking inputs ------------------------------------------------------------
  # checking the names of the imported database fixes
  if (!identical(names(database_fixes), c("id", "variable", "value"))) {
    stop("Expecting a database fix file with columns `id`, `variable`, and `value`.",
         call. = FALSE)
  }

  # checking all variables are in `data=`
  if (any(!unique(database_fixes$variable) %in% names(data))) {
    missing_vars <-
      unique(database_fixes$variable)[!unique(database_fixes$variable) %in% names(data)] %>%
      {glue::glue_collapse(sQuote(.), sep = ", ", last = ", and ")}
    stop(glue::glue("Columns {missing_vars} are not in `data`."), call. = FALSE)
  }

  # checking that there are no factor variables
  # if there are, give an error and suggest using character variables instead
  if(purrr::every(data %>%
                  dplyr::select(dplyr::intersect(database_fixes[["variable"]], names(data))),
                  ~!is.factor(.x)) == FALSE) {
    stop(
      glue::glue(
        "Factor variables cannot be updated. ",
        " To update factor variables, convert using `as.character()` first. ",
        "The following variables are factors: ",
        glue::glue_collapse(
          data %>% dplyr::select(dplyr::intersect(database_fixes[["variable"]], names(data))) %>%
            dplyr::select_if(is.factor) %>% names()),
        sep = ", "
      ),
      call. = FALSE
    )
  }

  # checking there are no duplicates in file
  dup_n <- database_fixes %>% dplyr::group_by(.data$id, .data$variable) %>%
    dplyr::mutate(n = dplyr::n()) %>% dplyr::pull(.data$n) %>% max()
  if (dup_n > 1) {
    stop("There are duplicates in the database fixes.", call. = FALSE)
  }

  # TODO: Note... there is no difference between a date and a datetime in POSIX dates.
  # The lubridate::as_datetime function gives you a POSIX object (because these can deal with times)
  # If there is a time associated with a date this will automatically be included in POSIX
  # TODO: Confirm that merging a "date" (from database fixes) with a "datetime" (from main data) works correctly

  # TODO: What happens if more than one check adds a different factor level...


  # setting up fixed data ------------------------------------------
  database_fixes2 <-
    database_fixes %>%
    dplyr::mutate(
      id_expr = purrr::map(.data$id, rlang::parse_expr),
      nrows_modified = purrr::map_int(
        .data$id_expr, ~with(data, eval(.x)) %>% sum()),
      variable_type = purrr::map_chr(.data$variable, ~ class(data[[.x]])[1]),
      value_type = purrr::pmap(
        list(.data$id, .data$variable, .data$variable_type, .data$value),
        function(id, variable, variable_type, value) {
          switch(
            variable_type,
            "character" = as.character(value),
            "logical" = as.logical(value),
            "integer" = fix_integer(variable, value, id),
            "numeric" = as.numeric(value),
            #"factor" = fix_factor($data, variable, value, id),
            "POSIXct" = fix_date(variable_type, variable, value, id),
            "POSIXlt" = fix_date(variable_type, variable, value, id),
            "POSIXt" = fix_date(variable_type, variable, value, id), # TODO: Is this necessary??
            "Date" = fix_date(variable_type, variable, value, id),
            "difftime" = fix_difftime(data, variable, value)
          ) %||% stop("ERROR")

        }
      )
    )
  database_fixes2

  # more checking --------------------------------------------------------------

  # Print a message if any IDs don't match
  if(any(database_fixes2$nrows_modified) == 0) {
    message(
      glue::glue(
        "The following IDs did not match any rows: ",
        glue::glue_collapse(
          database_fixes2 %>% dplyr::filter(nrows_modified == 0) %>% dplyr::pull(id),
          sep = ", ")))
  }

  # checking that each id identifies one row and only one row
  if(any(database_fixes2$nrows_modified > 1)) {
    stop("Each `id` must be associated with only one observation in the main data.",
         call. = FALSE)
  }

  # checking there is a value type of each correction
  values_error <- purrr::map_lgl(database_fixes2$value_type, is.na) %>% sum()

  if(sum(values_error) > 0) {
    print(database_fixes2[values_error, c("variable", "value")])
    stop(paste("Could not identify variable type/class.",
               "If the class is a Base R class, please file a GitHub issue."),
         call. = FALSE)
  }

  # checking expression in id results in a logical vector
  if (!purrr::every(database_fixes2$id_expr, ~is.logical(with(data, eval(.x))))) {
    stop("Each expression in `id` column must evaluate to a logical.", call. = FALSE)
  }

  # make changes in database ---------------------------------------------------

  # create updated dataset
  data_updated <- data
  for (i in 1:nrow(database_fixes2)) {
    data_updated[with(data, eval(database_fixes2$id_expr[[i]])), database_fixes2$variable[i]] <-
      database_fixes2$value_type[[i]]
  }

  # checking updated data against old data -------------------------------------
  if (!identical(names(data), names(data_updated))) {
    stop("Error in column names after updating data.", call. = FALSE)
  }

  if (!identical(purrr::map_chr(data, ~class(.x)[1]), purrr::map_chr(data_updated, ~class(.x)[1]))) {
    stop("Error where column class changed after updating data.", call. = FALSE)
  }

  # returning updated data ------------------------------------------------------
  data_updated
}

