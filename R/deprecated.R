#' Deprecated functions
#'
#' \lifecycle{deprecated}
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

# v0.5.0 (XXXX-XX-XX) ----------------------------------------------------------
#' @rdname deprecated
#' @export
create_hot_project <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.4.5",
    "bstfun::create_hot_project()",
    "bstfun::create_bst_project()"
  )
}

#' @rdname deprecated
#' @export
use_hot_file <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.4.5",
    "bstfun::use_hot_file()",
    "bstfun::use_bst_file()"
  )
}

#' @rdname deprecated
#' @export
use_hot_gitignore <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.4.5",
    "bstfun::use_hot_gitignore()",
    "bstfun::use_bst_gitignore()"
  )
}

#' @rdname deprecated
#' @export
use_hot_readme <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.4.5",
    "bstfun::use_hot_readme()",
    "bstfun::use_bst_readme()"
  )
}

#' @rdname deprecated
#' @export
use_hot_setup <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.4.5",
    "bstfun::use_hot_setup()",
    "bstfun::use_bst_setup()"
  )
}

#' @rdname deprecated
#' @export
use_hot_analysis <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.4.5",
    "bstfun::use_hot_analysis()",
    "bstfun::use_bst_analysis()"
  )
}

#' @rdname deprecated
#' @export
use_hot_report <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.4.5",
    "bstfun::use_hot_report()",
    "bstfun::use_bst_report()"
  )
}

# v0.4.5 (2022-06-21) ----------------------------------------------------------
#' @rdname deprecated
#' @export
reinstall_prior_pkgs <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.4.5",
    "bstfun::reinstall_prior_pkgs()",
    "updater::install_pkgs()"
  )
}

# v0.2.5 (2020-10-16) ----------------------------------------------------------
#' @rdname deprecated
#' @export
tbl_2way_summary <- function(data, row, col, con, label = NULL,
                             statistic = "{median} ({p25}, {p75})") {
  lifecycle::deprecate_stop("0.2.5", "bstfun::tbl_2way_summary()",
                            "gtsummary::tbl_continuous()")
}

#' @rdname deprecated
#' @export
gtsummary_butcher <- function(x) {
  lifecycle::deprecate_stop(
    when = "0.2.5", what = "bstfun::gtsummary_butcher()",
    with = "gtsummary::tbl_butcher()")
}

#' @rdname deprecated
#' @export
gts_add_p_footnotes <- function(x, printer = NULL, index_start = NULL) {
  lifecycle::deprecate_warn(when = "0.2.5", what = "bstfun::gts_add_p_footnotes()",
                            with = "gtsummary::separate_p_footnotes()")

  gtsummary::separate_p_footnotes(x)
}


# v0.1.5 (2020-04-16) ----------------------------------------------------------
tbl_ancova <- function(data, y, x, formula = "{y} ~ {x}", label = NULL,
                       method.args = NULL, conf.level = 0.95,
                       estimate_fun = NULL, pvalue_fun = NULL,
                       method = stats::lm, digits = NULL) {
  lifecycle::deprecate_stop("0.1.5", "bstfun::tbl_ancova()", "gtsummary::add_difference()")
}
