#' Get variable labels and store in named list
#'
#' @param data Data frame
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' list_labels(trial)
list_labels <- function(data) {
  map(data, ~ attr(.x, "label")) %>%
    purrr::compact()
}
