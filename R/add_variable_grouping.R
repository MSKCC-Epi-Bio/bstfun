#' Group variable summaries
#'
#' @description Some data are inherently grouped, and should be reported together.
#' For example, one person likely belongs to multiple racial groups and
#' the results of these tabulations
#' belong in a grouped portion of a summary table.
#'
#' Grouped variables are all indented together. The label row is a single
#' indent, and the other rows are double indented.
#'
#' @section Warning:
#' While the returned table is the same class as the input, it does not follow
#' the structure expected in other gtsummary functions that accept
#' these objects: errors may occur.
#'
#' @param x a gtsummary table
#' @param ... named arguments. The name is the group label that will be inserted
#' into the table. The values are character names of variables that will be grouped
#'
#' @return a gtsummary table
#' @export
#'
#' @examples
#' set.seed(11234)
#' add_variable_grouping_ex1 <-
#'   data.frame(
#'     race_asian = sample(c(TRUE, FALSE), 20, replace = TRUE),
#'     race_black = sample(c(TRUE, FALSE), 20, replace = TRUE),
#'     race_white = sample(c(TRUE, FALSE), 20, replace = TRUE),
#'     age = rnorm(20, mean = 50, sd = 10)
#'   ) %>%
#'   gtsummary::tbl_summary(
#'     label = list(race_asian = "Asian",
#'                  race_black = "Black",
#'                  race_white = "White",
#'                  age = "Age")
#'   ) %>%
#'   add_variable_grouping(
#'     "Race (check all that apply)" = c("race_asian", "race_black", "race_white")
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_variable_grouping_ex1.png}{options: width=50\%}}

add_variable_grouping <- function(x, ...) {
  if (!inherits(x, "gtsummary")) {
    stop("`x=` must be class 'gtsummary'.", call. = FALSE)
  }

  dots <- rlang::dots_list(...)
  if (!rlang::is_named(dots)) {
    stop("All arguments passed in `...` must be named.", call. = FALSE)
  }

  # insert a grouped variable header -------------------------------------------
  df_insert <-
    dots %>%
    imap(~which(x$table_body$variable %in% .x[1])[1]) %>%
    unlist() %>%
    tibble::enframe("include_group", "insert_above_row_number")

  for (i in rev(seq_len(nrow(df_insert)))) {
    x <-
      x %>%
      gtsummary::modify_table_body(
        ~ tibble::add_row(.x,
                          variable = df_insert$include_group[i],
                          label = df_insert$include_group[i],
                          row_type = "label",
                          .before = df_insert$insert_above_row_number[i])
      )
  }

  # update the indentation for the grouped variables ---------------------------
  x <-
    x %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(dots)) & .data$row_type %in% "label",
      text_format = "indent"
    ) %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(dots)) & !.data$row_type %in% "label",
      undo_text_format =  TRUE,
      text_format = "indent"
    ) %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(dots)) & !.data$row_type %in% "label",
      text_format = "indent2"
    )

  # return final tbl -----------------------------------------------------------
  x
}
