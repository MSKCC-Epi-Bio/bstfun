#' Convert Data Frame to gtsummary tbl
#'
#' @param data a data frame
#' @param column_labels logical indicating whether to use column label attributes.
#' Default is `TRUE`
#' @param bold_headers logical indicating whether to bold column headers.
#' Default is `TRUE`
#'
#' @return gtsummary object
#' @export
#' @family gtsummary-related functions
#'
#' @examples
#' as_gtsummary_ex1 <-
#'   head(gtsummary::trial) %>%
#'   as_gtsummary()
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{as_gtsummary_ex1.png}{options: width=75\%}}
as_gtsummary <- function(data, column_labels = TRUE, bold_headers = TRUE) {
  stopifnot(is.data.frame(data))

  # convert data frame to gtsummary table
  tbl <- gtsummary::.create_gtsummary_object(table_body = data)
  tbl$table_styling$header$hide <- FALSE # showing all columns by default
  tbl$table_styling$header$align <- "left" # showing all columns left aligned

  # use column labels
  if (isTRUE(column_labels)) {
    tbl$table_styling$header <-
      tbl$table_styling$header %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        label = attr(data[[.data$column]], "label") %||% .data$column
      ) %>%
      dplyr::ungroup()
  }

  # add markdown bold syntax
  if (isTRUE(bold_headers)) {
    tbl$table_styling$header$label <-
      paste0("**", tbl$table_styling$header$label, "**")
  }

  # return gtsummary tbl
  tbl
}
