#' Reduce size of gtsummary objects
#'
#' \lifecycle{deprecated}
#' Some gtsummary objects can become large and the size becomes cumbersome
#' when working with the object.
#' The function removes all elements from a gtsummary object, except those
#' required to print the table. This may result in gtsummary functions
#' that add information or modify the table, such as `add_global_p()`,
#' will no longer execute
#' after the excess elements have been removed (aka butchered). Of note,
#' the majority of `inline_text()` calls will continue to execute
#' properly.
#'
#' @param x a gtsummary object
#'
#' @return a gtsummary object
#' @family gtsummary-related functions
#' @export
#'
#' @examples
#' library(gtsummary)
#'
#' tbl_large <-
#'  trial %>%
#'  tbl_uvregression(
#'    y = age,
#'    method = lm
#'  )
#'
#'  tbl_butchered <-
#'    tbl_large %>%
#'    gtsummary_butcher()
#'
#'  # size comparison
#'  object.size(tbl_large) %>% format(units = "Mb")
#'  object.size(tbl_butchered) %>% format(units = "Mb")
gtsummary_butcher <- function(x) {
  lifecycle::deprecate_warn(
    when = "0.2.5", what = "bstfun::gtsummary_butcher()",
    with = "gtsummary::tbl_butcher()")

  gtsummary::tbl_butcher(x)
}
