#' Assess pattern of missing data
#'
#' Pass a data frame and the missing pattern of all columns in the data frame.
#' The data frame is returned unmodified.
#'
#' @param data data frame
#' @param include character vector of names to include
#' @param exclude character vector of names to exclude
#'
#' @return original data frame invisibly returned
#' @export
#' @examples
#' trial %>% count_na()
count_na <- function(data, include = NULL, exclude = NULL) {
  if (is.null(include)) include <- names(data)
  include <- include %>% setdiff(exclude)

  cli::cli_text("TRUE = {.val Available}, FALSE = {.val Not Available}")
  cat("\n")
  data[include] %>%
    dplyr::mutate_all(~ !is.na(.)) %>%
    dplyr::group_by_all() %>%
    dplyr::count() %>%
    as.data.frame() %>%
    print()

  invisible(data)
}
