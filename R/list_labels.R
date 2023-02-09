#' Get variable labels and store in named list
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use `map(data, ~ attr(.x, "label"))` instead.
#' @param data Data frame
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' list_labels(trial)
list_labels <- function(data) {

  lifecycle::deprecate_soft("0.5.2", "list_labels()",
                            details = "Please use `map(data, ~ attr(.x, 'label'))`")

  map(data, ~ attr(.x, "label")) %>%
    purrr::compact()
}
