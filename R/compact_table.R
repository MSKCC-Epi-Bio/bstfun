#' Compact Table Styling
#'
#' Apply the same compact styling available from
#' `gtsummary::theme_gtsummary_compact()` to any
#' {gt}, {flextable}, {huxtable}, or `knitr::kable()` table.
#' `knitr::kable()` stlying uses the {kableExtra} package
#' @param data a {gt}, {flextable}, {huxtable}, or `knitr::kable()` table.
#' @export
#'
#' @examples
#' compact_table_ex1 <-
#'   head(trial) %>%
#'   gt::gt() %>%
#'   compact_table()
compact_table <- function(data) {
  # first extract the names of the commands that make tables compact
  # from the `gtsummary::theme_gtsummary_compact()` theme function
  if (inherits(data, "gt_tbl")) cmd_names <- "as_gt-lst:addl_cmds"
  else if (inherits(data, "flextable")) cmd_names <- "as_flex_table-lst:addl_cmds"
  else if (inherits(data, "huxtable")) cmd_names <- "as_hux_table.gtsummary-lst:addl_cmds"
  else if (inherits(data, "knitr_kable")) {
    cmd_names <- "as_kable_extra-lst:addl_cmds"
    if (!requireNamespace("kableExtra"))
      rlang::abort("Styling a `knitr::kable()` table requires the {kableExtra} package.")
  }
  else rlang::abort("`data=` must be a {gt}, {flextable}, {huxtable}, or `knitr::kable()` table.")


  # extract the compact command expressions and
  # concatenate them into a single expression and evaluate
  gtsummary::theme_gtsummary_compact(set_theme = FALSE) %>%
    purrr::pluck(cmd_names) %>%
    unlist() %>%
    purrr::compact() %>% # removes null elements of list (there shouldn't be any)
    {c(list(data), .)} %>%
    purrr::reduce(function(x, y) rlang::expr(!!x %>% !!y)) %>%
    eval()
}

