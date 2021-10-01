#' Split gtsummary Table
#'
#' @param x gtsummary table
#' @param variables variables to split gtsummary table at
#' @param ... not used
#'
#' @return tbl_split object
#' @name tbl_split
#'
#' @family gtsummary-related functions
#' @examples
#' library(gtsummary)
#'
#' tbl <-
#'   tbl_summary(trial) %>%
#'   tbl_split(variables = c(marker, grade))
#'
#' @section Possible Changes:
#' - Function may be migrated to the gtsummary package
#' - May update print method within R markdown to add a page break between tables
NULL

#' @export
#' @rdname tbl_split
tbl_split <- function(x, variables) {
  lifecycle::deprecate_warn(when = "0.2.5", what = "bstfun::tbl_split()",
                            with = "gtsummary::tbl_split()")

  gtsummary::tbl_split(x, variables)

  }

#' @export
#' @rdname tbl_split
print.tbl_split <- function(x, ...) {
  purrr::walk(x, print)
}

