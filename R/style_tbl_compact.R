#' Compact Table Styling
#'
#' Apply the same compact styling available from
#' `gtsummary::theme_gtsummary_compact()` to any
#' {gt}, {flextable}, {huxtable}, or `knitr::kable()` table.
#' `knitr::kable()` stlying uses the {kableExtra} package
#' @param data a {gt}, {flextable}, {huxtable}, or `knitr::kable()` table.
#' @export
#'
#' @family gtsummary-related functions
#' @examples
#' style_tbl_compact_ex1 <-
#'   head(trial) %>%
#'   gt::gt() %>%
#'   style_tbl_compact()
style_tbl_compact <- function(data) {
  # first extract the theme element names of the commands that make tables compact
  # from the `gtsummary::theme_gtsummary_compact()` theme function
  if (inherits(data, "gt_tbl")) theme_name <- "as_gt-lst:addl_cmds"
  else if (inherits(data, "flextable"))  {
    assert_package("flextable", "style_tbl_compact()")
    theme_name <- "as_flex_table-lst:addl_cmds"
  }
  else if (inherits(data, "huxtable")) {
    assert_package("huxtable", "style_tbl_compact()")
    theme_name <- "as_hux_table.gtsummary-lst:addl_cmds"
  }
  else if (inherits(data, "knitr_kable")) {
    assert_package("kableExtra", "style_tbl_compact()")
    theme_name <- "as_kable_extra-lst:addl_cmds"
  }
  else rlang::abort("`data=` must be a {gt}, {flextable}, {huxtable}, or `knitr::kable()` table.")


  # extract the compact command expressions and
  # concatenate them into a single expression and evaluate
  gtsummary::theme_gtsummary_compact(set_theme = FALSE) %>%
    purrr::pluck(theme_name) %>%
    unlist() %>%
    purrr::compact() %>% # removes null elements of list (there shouldn't be any)
    purrr::reduce(function(x, y) rlang::expr(!!x %>% !!y), .init = data) %>%
    eval()
}

