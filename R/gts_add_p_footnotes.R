#' Moves p-value footnotes to individual p-values
#'
#' @param x object with class `"tbl_summary"` created from the gtsummary package
#' @param printer DEPRECATED
#' @param index_start DEPRECATED
#'
#' @export
#' @family gtsummary-related functions
#' @examples
#' # library(gtsummary)
#' # gts_add_p_footnotes_ex1 <-
#'   # trial %>%
#'   # select(trt, age, grade) %>%
#'   # tbl_summary(by = trt) %>%
#'   # add_p() %>%
#'   # gts_add_p_footnotes()
#'
#' @section Example Output:
#' \if{html}{\figure{gts_add_p_footnotes_ex1.png}{options: width=80\%}}

gts_add_p_footnotes <- function(x, printer = NULL, index_start = NULL) {
  lifecycle::deprecate_warn(when = "0.2.5", what = "bstfun::gts_add_p_footnotes()",
                            with = "gtsummary::separate_p_footnotes()")

  gtsummary::separate_p_footnotes(x)
}



