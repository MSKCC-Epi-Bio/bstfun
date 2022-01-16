#' Summarize Checkbox-type Data
#'
#' Some data are inherently grouped, and should be reported together.
#' For example, race one person may belong to multiple racial groups, e.g.
#' part Asian and part Caucasian, and the results of these tabulations
#' belong in a grouped portion of a summary table.
#'
#' @section Warning:
#' While the returned table is of class 'tbl_summary', it does not follow
#' the structure expected in other gtsummary functions that accept
#' 'tbl_summary' class objects: errors may occur.
#'
#' @param data a data frame
#' @param include a list including each variable that will be summarized
#' in the resulting table. Named elements will be grouped together in the output,
#' and each grouped variable must be dichotomous, e.g.
#' `list("age", "Race (check all that apply)" = c("race_white", "race_black", "race_asian"))`
#' @inheritDotParams gtsummary::tbl_summary -include -data
#'
#' @return a gtsummary table
#' @export
#'
#' @examples
#' set.seed(11234)
#' tbl_checkbox_ex1 <-
#'   data.frame(
#'     race_asian = sample(c(TRUE, FALSE), 20, replace = TRUE),
#'     race_black = sample(c(TRUE, FALSE), 20, replace = TRUE),
#'     race_white = sample(c(TRUE, FALSE), 20, replace = TRUE),
#'     age = rnorm(20, mean = 50, sd = 10)
#'   ) %>%
#'   tbl_checkbox(
#'     include =
#'       list("Race (check all that apply)" = c("race_asian", "race_black", "race_white"),
#'            "age"),
#'     label = list(race_asian = "Asian",
#'                  race_black = "Black",
#'                  race_white = "White",
#'                  age = "Age")
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_checkbox_ex1.png}{options: width=50\%}}

tbl_checkbox <- function(data, include, ...) {
  # checking inputs ------------------------------------------------------------
  if (!rlang::is_list(include)) {
    stop("Argument `include=` must be a named list.", call. = FALSE)
  }

  include_groups <- purrr::discard(names(include), ~ . == "")
  if (rlang::is_empty(include_groups)) {
    stop("Argument `include=` must include at least one named group of variables.",
         call. = FALSE)
  }

  # building base table --------------------------------------------------------
  tbl <-
    gtsummary::tbl_summary(
      data = data,
      include = unlist(include) %>% unname(),
      ...
    )

  # check that all grouped variables are dichotomous ---------------------------
  grouped_variables <- include[include_groups] %>% unlist() %>% unname()
  grouped_variables_are_all_dichotomous <-
    tbl$meta_data %>%
    dplyr::filter(.data$variable %in% grouped_variables) %>%
    dplyr::pull(.data$summary_type) %>%
    purrr::every(~ .x %in% "dichotomous")
  if (!grouped_variables_are_all_dichotomous) {
    stop("All grouped variables in `include=` must be dichotomous.", call. = FALSE)
  }

  # insert a grouped variable header -------------------------------------------
  df_insert <-
    include[include_groups] %>%
    imap(~which(tbl$table_body$variable %in% .x[1])[1]) %>%
    unlist() %>%
    tibble::enframe("include_group", "insert_above_row_number")

  for (i in rev(seq_len(nrow(df_insert)))) {
    tbl <-
      tbl %>%
      gtsummary::modify_table_body(
        ~ tibble::add_row(.x,
                          variable = df_insert$include_group[i],
                          label = df_insert$include_group[i],
                          row_type = "label",
                          .before = df_insert$insert_above_row_number[i])
      )
  }

  # update the indentation for the grouped variables ---------------------------
  tbl <-
    tbl %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(include[include_groups])) & .data$row_type %in% "label",
      text_format = "indent"
    ) %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(include[include_groups])) & !.data$row_type %in% "label",
      undo_text_format =  TRUE,
      text_format = "indent"
    ) %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(include[include_groups])) & !.data$row_type %in% "label",
      text_format = "indent2"
    )

  # return final tbl -----------------------------------------------------------
  class(tbl) <- c("tbl_checkbox", class(tbl))
  tbl
}
