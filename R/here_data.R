#' Find your data folder
#'
#' Similar to `here::here()` which returns your project directory, `here_data()`
#' returns the path to your current data folder based on the date in `data_date.txt`
#'
#' The function expects the user to version their data using a text file indicating
#' the date the data was last received, and the data to be stored in a corresponding
#' folder name, e.g. `~\Project Folder\secure_data\2020-01-01`.
#'
#' @inheritParams here::here
#' @param data_folder_name name of data folder. Default is `"secure_data"`
#'
#' @return path to data folder
#' @export
#'
#' @examples
#' \dontrun{
#' here_data()
#' #> "C:\Users\SjobergD\GitHub\My Project\secure_data\2020-01-01"
#'
#' here_data("Raw Data.xlsx")
#' #> "C:\Users\SjobergD\GitHub\My Project\secure_data\2020-01-01\Raw Data.xlsx"
#' }
here_data <- function(..., data_folder_name = "secure_data") {
  # first looking for data_date file -------------------------------------------
  potential_filenames <- c("data_date.txt", "data_date", "dataDate.txt", "dataDate")
  data_date_file <-
    map_lgl(potential_filenames, ~ fs::is_file(here::here(.x))) %>%
    which() %>%
    {purrr::pluck(potential_filenames, .[1])}
    here::here()

  # error if no file found -----------------------------------------------------
  if (is.null(data_date_file))
    paste("No text file containing the data date could be found. Expecting one of",
          paste(shQuote(potential_filenames, type = "csh"), collapse = ", "),
          "in the project's root directomry.") %>%
    stringr::str_wrap() %>%
    stop(call. = FALSE)

  # returning full data path ---------------------------------------------------
  here::here(data_folder_name, readr::read_lines(data_date_file, n_max = 1), ...)
}
