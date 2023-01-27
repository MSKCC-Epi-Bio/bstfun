#' Add inline forest plot
#'
#' This function works with HTML output from the gt package only.
#' Adds an in-line forest plot to a summary table.
#'
#' *Estimates from `tbl_regression()` and `tbl_uvregression()` that have
#' been exponentiated are shown on the log scale.*
#'
#' @param x a gtsummary object
#' @param header string indicating column header of new forest plot column.
#' Default is `"**Forest Plot**"`.
#' @param spec_pointrange.args named list of arguments that will be passed to
#' `kableExtra::spec_pointrange()`. Use this argument to modify the
#' default ascetics of the forest plot, e.g. color, size, symbols, etc.
#' Default is `list(width = 250, cex = .75, col = "black", pch = 16)`
#'
#' @return gtsummary object
#' @family gtsummary-related functions
#' @export
#'
#' @examples
#' library(gtsummary)
#'
#' # Example 1 ----------------------------------
#' add_inline_forest_plot_ex1 <-
#'   lm(mpg ~ cyl + am + drat, mtcars) %>%
#'   tbl_regression() %>%
#'   add_inline_forest_plot()
#'
#' @section Example Output:
#' \if{html}{\figure{add_inline_forest_plot_ex1.png}{options: width=80\%}}

add_inline_forest_plot <- function(x, header = "**Forest Plot**",
                                   spec_pointrange.args = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) rlang::abort("`x=` must be class 'gtsummary'")
  if (!all(c("estimate", "conf.low", "conf.high") %in% names(x$table_body)))
    rlang::abort("`x$table_body` must contain columns 'estimate', 'conf.low', 'conf.high'")
  assert_package("kableExtra", "add_inline_forest_plot()")
  updated_call_list <- c(x$call_list, list(add_inline_forest_plot = match.call()))

  # if exponentiated, plot on the log scale ------------------------------------
  scale_fun <-
    switch(
      inherits(x, c("tbl_regression", "tbl_uvregression")) && x$inputs$exponentiate,
      log
    ) %||%
    identity

  # add column with forest plot ------------------------------------------------
  # prepping arguments for `kableExtra::spec_pointrange()`
  spec_pointrange.args <-
    list(vline = 0, width = 250, cex = .75, col = "black", pch = 16) %>%
    purrr::list_modify(!!!spec_pointrange.args) %>%
    purrr::list_modify(x = scale_fun(x$table_body$estimate),
                       xmin = scale_fun(x$table_body$conf.low),
                       xmax = scale_fun(x$table_body$conf.high))

  x <-
    gtsummary::modify_table_body(
      x = x,
      ~.x %>%
        # construct the forest plots and add to `x$table_body`
        dplyr::bind_cols(
          tibble::tibble(
            forest_plot =
              rlang::inject(kableExtra::spec_pointrange(!!!spec_pointrange.args)) %>%
              purrr::map("svg_text") %>%
              purrr::map(~gt::html(as.character(.x)))
          )
        ) %>%
        # move forest plot to before the coef
        dplyr::relocate("forest_plot", .before = "estimate") %>%
        # remove empty forest plots
        dplyr::mutate(forest_plot = map2(.data$estimate, .data$forest_plot,
                                         ~switch(!is.na(.x), .y)))
    ) %>%
    gtsummary::modify_table_styling(
      columns = "forest_plot",
      hide = FALSE,
      label = header
    )

  # return updated gtsummary object --------------------------------------------
  x$call_list <- updated_call_list
  x
}

assert_package <- function(pkg, fn = NULL, boolean = FALSE) {
  broom.helpers::.assert_package(pkg = pkg, fn = fn,
                                 pkg_search = "bstfun", boolean = boolean)
}
