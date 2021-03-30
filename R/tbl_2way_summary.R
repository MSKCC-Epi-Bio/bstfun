#' Two-way Summary
#'
#' Summarize a continuous variable by two categorical variables
#'
#' @param data a data frame
#' @param row column name from `data=` making the rows of the resulting table
#' @param col column name from `data=` making the columns of the resulting table
#' @param con column name from `data=`of a continuous variable that will be
#' summarized in each cell of the table
#' @param label list of formulas specifying variables labels
#' @param statistic string indicating the summary statistics. Default
#' is `"{median} ({p25}, {p75})"`
#'
#' @return a gtsummary table
#' @export
#'
#' @examples
#' tbl <-
#'   trial %>%
#'   tbl_2way_summary(grade, trt, marker)

tbl_2way_summary <- function(data, row, col, con, label = NULL,
                             statistic = "{median} ({p25}, {p75})") {
  row <- dplyr::select(data, {{ row }}) %>% names()
  col <- dplyr::select(data, {{ col }}) %>% names()
  con <- dplyr::select(data, {{ con }}) %>% names()

  label = broom.helpers::.formula_list_to_named_list(label, data = data)
  lbl_col <- label[[col]] %||% attr(data[[col]], "label") %||% col
  lbl_col <- stringr::str_glue("**{lbl_col}**")
  lbl_row <- label[[row]] %||% attr(data[[row]], "label") %||% row


  data %>%
    dplyr::select(all_of(c(row, col, con))) %>%
    dplyr::arrange(!!rlang::sym(row)) %>%
    tidyr::nest(data = all_of(c(col, con))) %>%
    dplyr::rename(row = !!rlang::sym(row)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      lbl_formula =
        list(as.character(.data$row)) %>% rlang::set_names(.env$con),
      tbl =
        data %>%
        # build summary table, and use the grade level as the label
        gtsummary::tbl_summary(by = .env$col,
                               label = list(as.character(.data$row)) %>% rlang::set_names(.env$con),
                               statistic = list(statistic) %>% rlang::set_names(.env$con),
                               missing = "no") %>%
        gtsummary::modify_header(gtsummary::all_stat_cols() ~ "{level}") %>%
        list()
    ) %>%
    dplyr::pull(.data$tbl) %>%
    gtsummary::tbl_stack() %>%
    gtsummary::modify_spanning_header(
      rlang::expr(gtsummary::all_stat_cols() ~ !!lbl_col) %>% stats::as.formula()
    ) %>%
    gtsummary::modify_table_body(
      ~.x %>%
        dplyr::mutate(
          row_type = "level"
        ) %>%
        {dplyr::bind_rows(
          tibble::tibble(
            label = .env$lbl_row,
            row_type = "label"
          ),
          .
        )} %>%
        dplyr::mutate(
          variable = .env$row,
          var_type = "categorical",
        )
    ) %>%
    gtsummary::bold_labels()
}



