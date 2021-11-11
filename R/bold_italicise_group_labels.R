
#' Set bold and/or italic style for groups labels in stacked tables
#'
#' @param x a gtsummary stacked table
#' @param bold logical indicating whether to bold the group header rows
#' @param italics logical indicating whether to italicize the group header rows
#' @param print_engine Choose a print engine to render the table, must be one of
#' `c("gt", "flextable", "huxtable")`
#' @return A table of class selected in print_engine. Of note, the output will no longer
#' be a gtsummary table.
#' @export
#' @family gtsummary-related functions
#' @examples
#' library(gtsummary)
#'
#' bold_italicise_group_labels_ex1 <-
#'   trial %>%
#'   select(age, trt, grade) %>%
#'   tbl_strata(
#'     strata = grade,
#'     ~ .x %>%
#'       select(trt, age) %>%
#'       tbl_summary(by = trt, missing = "no"),
#'     .combine_with = "tbl_stack"
#'   ) %>%
#'   bold_italicise_group_labels(bold = TRUE)
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{bold_italicise_group_labels_ex1.png}{options: width=50\%}}

bold_italicise_group_labels <- function(x,
                                        bold = FALSE,
                                        italics = FALSE,
                                        print_engine = c("gt", "flextable", "huxtable")) {
  # input checks ---------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Class of 'x' must be 'gtsummary'", call. = FALSE)
  }
  print_engine <- match.arg(print_engine)

  # convert output to print_engine type ----------------------------------------
  x <-
    switch(
      print_engine,
      "gt" = gtsummary::as_gt(x),
      "flextable" = gtsummary::as_flex_table(x),
      "huxtable" = gtsummary::as_hux_table(x)
    )
  cli::cli_alert_info("{.field gtsummary} table has been converted class {.val {print_engine}}")

  # huxtable rows to format ----------------------------------------------------
  # huxtables contains a dataframe with named rows,
  # each row starting with a dot is header or footer
  # with the following regex in argument "row" we
  # unselect such a rows (headers or footers)
  if (identical(print_engine, "huxtable")) {
    rows_to_format <-
      stringr::str_detect(pattern = "\\.",
                          string = rownames(x),
                          negate = TRUE)
    rows_to_format[1] <- FALSE
  }

  # apply bold code to table ---------------------------------------------------
  # THE GROUP COLUMNS WILL ALWAYS BEGIN WITH 'groupname_col*'
  # APPLY THE FORMATTING TO THOSE COLUMNS
  if (bold == TRUE) {
    x <-
      switch(
        print_engine,
        "gt" =
          x %>% gt::tab_options(row_group.font.weight = "bold"),
        "flextable" =
          x %>% flextable::bold(j = stringr::str_starts(string = .$col_keys,
                                                        pattern = "groupname_col")),
        "huxtable" =
          x %>% huxtable::set_bold(row = rows_to_format,
                                   col = dplyr::starts_with("groupname_col"))
      )
  }

  # apply italics code to table ---------------------------------------------------
  if (italics == TRUE) {
    x <-
      switch(
        print_engine,
        "gt" =
          x %>% gt::tab_style(style = gt::cell_text(style = "italic"),
                              locations = gt::cells_row_groups()),
        "flextable" =
          x %>% flextable::italic(j = stringr::str_starts(string = .$col_keys,
                                                          pattern = "groupname_col")),
        "huxtable" =
          x %>% huxtable::set_italic(row = rows_to_format,
                                     col = dplyr::starts_with("groupname_col"))
      )
  }

  # return formatted table ----------------------------------------------------
  x
}
