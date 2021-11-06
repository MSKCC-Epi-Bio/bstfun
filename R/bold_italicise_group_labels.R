
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

  # apply bold code to table ---------------------------------------------------
  # THE GROUP COLUMNS WILL ALWAYS BEGIN WITH 'groupname_col*'
  # APPLY THE FORMATTING TO THOSE COLUMNS
  if (bold == TRUE) {
    switch(
      print_engine,
      "gt" = {
        x <- gtsummary::as_gt(x)
        x <-
          x %>%
          gt::tab_options(row_group.font.weight = "bold")
      },
      "flextable" = {
        x <- gtsummary::as_flex_table(x)
        x <-
          x %>%
          flextable::bold(j = stringr::str_starts(string = .$col_keys, pattern = "groupname_col"))

      },
      "huxtable" =  {
        x = gtsummary::as_hux_table(x)
        x <-
          x %>%
          huxtable::set_bold(
            row = stringr::str_detect(
              pattern = "\\.",
              string = rownames(.),
              negate = TRUE
            ),
            col = dplyr::starts_with("groupname_col")
          )
      }
    )
    cli::cli_alert_info("{.field gtsummary} table has been converted class {.val {print_engine}}")
  }

  # apply italics code to table ---------------------------------------------------

  if (italics == TRUE) {
    switch(
      print_engine,
      "gt" = {
        if (!("gt_tbl" %in% class(x))) {
          x <- gtsummary::as_gt(x)
        }
        x <-
          x %>%
          gt::tab_style(style = cell_text(style = "italic"),
                        locations = cells_row_groups())
      },
      "flextable" = {
        if (!("flextable" %in% class(x))) {
          x <- gtsummary::as_flex_table(x)
        }

        x <- x %>%
          flextable::italic(j = stringr::str_starts(string = .$col_keys, pattern = "groupname_col"))
      },
      "huxtable" =  {
        if (!("huxtable" %in% class(x))) {
          x <- gtsummary::as_hux_table(x)
        }
        rowsToItalic <-
          stringr::str_detect(pattern = "\\.",
                              string = rownames(x),
                              negate = TRUE)
        rowsToItalic[1] <- FALSE
        x <-
          x %>%
          # huxtables contains a dataframe with named rows,
          # each row starting with a dot is header or footer
          # with the following regex in argument "row" we
          # unselect such a rows (headers or footers)
          huxtable::set_italic(row = rowsToItalic,
                               col = dplyr::starts_with("groupname_col"))
      }
    )
    cli::cli_alert_info("{.field gtsummary} table has been converted class {.val {print_engine}}")
  }
  x
}
