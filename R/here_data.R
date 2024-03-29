#' Find your data folder
#'
#' Uses `data_date.txt` to create a path with the data date populated.
#' - `here_data()`: Returns `here::here("secure_data", data_date, ...)`
#' - `path_data()`: Returns `fs::path(path, "secure_data", data_date, ...)`
#'
#' The function expects the user to version their data using a text file indicating
#' the date the data was last received, and the data to be stored in a corresponding
#' folder name, e.g. `~/Project Folder/secure_data/2020-01-01`.
#'
#' @param ... Path components to be appended to the end of the returned path string.
#' @param data_folder_name name of data folder. Default is `"secure_data"`
#' @param path path to folder where data is saved, e.g. `fs::path(path, )`
#' Default value is `getOption("path_data")` where `path_data` can be initialized
#'  at the top of the script with `options(path_data = "H:/.../data")`
#' @param path_to_data_date path to data date folder or file. If folder is passed,
#' expecting the data date file to be named one of
#' `c("data_date.txt", "data_date", "dataDate.txt", "dataDate")`.
#' Default value is `here::here()`
#'
#' @rdname here_data
#' @return path to data folder
#' @export
#'
#' @examples
#' if (FALSE) {
#' here_data()
#' #> "C:/Users/SjobergD/GitHub/My Project/secure_data/2020-01-01"
#'
#' here_data("Raw Data.xlsx")
#' #> "C:/Users/SjobergD/GitHub/My Project/secure_data/2020-01-01/Raw Data.xlsx"
#'
#' path_data(path = "O:/My Project", "Raw Data.xlsx")
#' #> "O:/My Project/secure_data/2020-01-01/Raw Data.xlsx"
#' }
here_data <- function(...,
                      data_folder_name = "secure_data",
                      path_to_data_date = here::here()) {
  # import data date -----------------------------------------------------------
  data_date <- get_data_date(path_to_data_date)

  # returning full data path ---------------------------------------------------
  here::here(data_folder_name, data_date, ...)
}

#' @rdname here_data
#' @export
path_data <- function(...,
                      path = getOption("path_data"),
                      data_folder_name = "secure_data",
                      path_to_data_date = here::here()) {
  # check path_data is initialized ---------------------------------------------
  if(is.null(path)){
    stop("path argument is NULL", call. = FALSE)
  }

  # import data date -----------------------------------------------------------
  data_date <- get_data_date(path_to_data_date)

  # returning full data path ---------------------------------------------------
  fs::path(path, data_folder_name, data_date, ...)
}

#' @rdname here_data
#' @export
get_data_date <- function(path_to_data_date = here::here()) {
  # first looking for data_date file -------------------------------------------
  if (fs::is_dir(path_to_data_date)) {
    potential_filenames <- c("data_date.txt", "data_date", "dataDate.txt", "dataDate")
    data_date_file <-
      map_lgl(potential_filenames, ~ fs::is_file(file.path(path_to_data_date, .x))) %>%
      which() %>%
      {purrr::pluck(potential_filenames, .[1])}

    # error if no file found ---------------------------------------------------
    if (is.null(data_date_file)) {
      paste("No text file containing the data date could be found. Expecting one of",
            paste(shQuote(potential_filenames, type = "csh"), collapse = ", "),
            "in the project's root directory.") %>%
      stringr::str_wrap() %>%
      stop(call. = FALSE)
    }

    path_to_data_date <- file.path(path_to_data_date, data_date_file)
  }

  # importing the data date ----------------------------------------------------
  data_date <-
    tryCatch(
      readr::read_lines(path_to_data_date, skip_empty_rows = TRUE),
      error = function(e) {
        cli::cli_alert_danger("There was an error importing data date from file {.path {path_to_data_date}}")
        stop(as.character(e))
      }
    )

  if (rlang::is_empty(data_date) || !rlang::is_string(data_date)) {
    "Expecting imported data date to be a string of length one and it is not." %>%
      stop(call. = FALSE)
  }

  data_date
}
