#' Add inline forest plot
#'
#' This function works with HTML output from the gt package only.
#' Adds an in-line forest plot to a summary table.
#'
#' @param x a gtsummary object
#'
#' @return gtsummary object
#' @export
#'
#' @examples
#' library(gtsummary)
#'
#' add_inline_forest_plot_ex1 <-
#'   lm(mpg ~ cyl + am + drat, mtcars) %>%
#'   tbl_regression() %>%
#'   add_inline_forest_plot()
#'
#' @section Example Output:
#' \if{html}{\figure{add_inline_forest_plot_ex1.png}{options: width=80\%}}

add_inline_forest_plot <- function(x) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) abort("`x=` must be class 'gtsummary'")
  if (!all(c("estimate", "conf.low", "conf.high") %in% names(x$table_body)))
    abort("`x$table_body` must contain columns 'estimate', 'conf.low', 'conf.high'")
  assert_package("kableExtra", "add_inline_forest_plot()")
  updated_call_list <- c(x$call_list, list(add_inline_forest_plot = match.call()))

  # set NULL value for plot ----------------------------------------------------
  null_value <-
    case_when(
      inherits(x, c("tbl_regression", "tbl_uvregression")) ~
        ifelse(x$inputs$exponentiate, 1L, 0L),
      TRUE ~ 0L
    )

  # add column with forest plot ------------------------------------------------
  x <-
    gtsummary::modify_table_body(
      x = x,
      ~.x %>%
        dplyr::bind_cols(
          tibble::tibble(
            forest_plot =
              kableExtra::spec_pointrange(
                x = .x$estimate,
                xmin = .x$conf.low,
                xmax = .x$conf.high,
                vline = null_value,
                width = 250,
                cex = .75,
                col = "black",
                pch = 16
              ) %>%
              purrr::map("svg_text") %>%
              purrr::map(~gt::html(as.character(.x)))
          )
        ) %>%
        dplyr::relocate(.data$forest_plot, .before = .data$estimate) %>%
        dplyr::mutate(forest_plot = map2(.data$estimate, .data$forest_plot,
                                         ~switch(!is.na(.x), .y)))
    ) %>%
    gtsummary::modify_table_styling(
      columns = "forest_plot",
      hide = FALSE,
      label = "**Forest Plot**"
    )

  # return updated gtsummary object --------------------------------------------
  x$call_list <- updated_call_list
  x
}
