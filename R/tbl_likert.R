#' Likert Summary
#'
#' @export
tbl_likert <- function(data) {
  # all variables in data must be a factor with the same levels ----------------
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
          percent = "row",
          label = list(...all_true... = (attr(data[[.x]], "label") %||% .x))
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
