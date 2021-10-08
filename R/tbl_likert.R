#' Likert Summary
#'
#' @inheritParams gtsummary::tbl_summary
#' @export
#' @examples
#' df <-
#'   tibble::tibble(
#'     f1 =
#'       sample.int(100, n = 3, replace = TRUE) %>%
#'       factor(levels = 1:3, labels = c("bad", "meh", "good")),
#'     f2 =
#'       sample.int(100, n = 3, replace = TRUE) %>%
#'       factor(levels = 1:3, labels = c("bad", "meh", "good")),
#'   )
#'
#' tbl_likert(df) %>%
#'   gtsummary::as_kable()
tbl_likert <- function(data,
                       label = NULL, statistic = NULL, digits = NULL,
                       include = everything()) {
  # evaluate inputs ------------------------------------------------------------
  include <-
    broom.helpers::.select_to_varnames( {{ include }},
                                        data = data, arg_name = "include")
  label <-
    broom.helpers::.formula_list_to_named_list(
      label,
      data = data,
      arg_name = "label"
    )
  statistic <-
    broom.helpers::.formula_list_to_named_list(
      statistic,
      data = data,
      arg_name = "statistic"
    )
  digits <-
    broom.helpers::.formula_list_to_named_list(
      digits,
      data = data,
      arg_name = "digits"
    )

  # all variables in data must be a factor with the same levels ----------------
  data <- data[include]
  data %>%
    map(
      function(x) {
        if (!inherits(x, "factor"))
          stop("All columns in `data=` must be class 'factor'.", call. = FALSE)
        if (!identical(attr(x, "levels"), attr(data[[1]], "levels")))
          stop("All columns must have the same factor levels.")
      }
    )

  # create summary table, then stack -------------------------------------------
  names(data) %>%
    setdiff("...all_true...") %>%
    purrr::map(
      ~ gtsummary::tbl_summary(
          data = data %>% dplyr::mutate(...all_true... = TRUE),
          by = all_of(.x),
          include = ...all_true...,
          type = list(...all_true... = "dichotomous"),
          statistic = list(...all_true... = statistic[[.x]]),
          percent = "row",
          digits = list(...all_true... = digits[[.x]]),
          label = list(...all_true... = (label[[.x]] %||% attr(data[[.x]], "label") %||% .x))
        ) %>%
        gtsummary::modify_header(gtsummary::all_stat_cols() ~ "**{level}**") %>%
        gtsummary_butcher() %>%
        gtsummary::modify_table_body(
          function(table_body) {
            table_body %>% dplyr::mutate(variable = .x)
          }
        )
    ) %>%
    gtsummary::tbl_stack(quiet = TRUE)
}
