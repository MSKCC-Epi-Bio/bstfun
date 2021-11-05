#' Check variable derivations
#'
#' Function assists in checking the values of new, derived variables against
#' the raw, source variables.
#'
#' @param data data frame
#' @param ... sets of variables to check. variables that are checked together
#' are included in the same vector. See example below.
#' @export
#' @examples
#' count_map(
#'   mtcars,
#'   c(cyl, am), c(gear, carb)
#' )
count_map <- function(data, ...) {
  # checking inputs
  stopifnot(is.data.frame(data))
  dots_list_quo <- rlang::enquos(...)

  dots_list_quo %>%
    purrr::walk(
      function(.x) {
        select(data, !!.x) %>%
          names() %>%
          {count_one(data, vars = .)}
      }
    )

  invisible()
}

count_one <- function(data, vars) {

  # printing variable name being checked
  cat(vars[1], "\n")

  # printing unique obs
  data %>%
    dplyr::count(!!!rlang::syms(vars), name = "..n..") %>%
    dplyr::mutate(
      ..p.. = gtsummary::style_percent(.data$..n.. / sum(.data$..n..), symbol = TRUE)
    ) %>%
    dplyr::arrange(!!!rlang::syms(vars)) %>%
    as.data.frame() %>%
    print(., row.names = FALSE)

  # adding line break
  cat("\n")
}
