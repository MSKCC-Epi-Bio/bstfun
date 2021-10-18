#' Create Forest Plot
#'
#' \lifecycle{experimental}
#' This function takes a gtsummary table and converts it to a
#' forest plot using `forestplot::forestplot()`.
#'
#' @param x a gtsummary object of class `"tbl_regression"` or `"tbl_uvregression"`
#' @param col_names names of columns in `x$table_body` to print on the RHS of the
#' forest plot. Default is `c("estimate", "ci", "p.value")`
#' @param title_line_color color of line that appears above forest plot.
#' Default is `"darkblue"`
#' @inheritParams forestplot::forestplot
#' @param ... arguments passed to `forestplot::forestplot()`
#'
#' @author Christine Zhou
#' @export
#' @examples
#' library(gtsummary)
#' library(survival)
#'
#' # Example 1 ----------------------------------
#' tbl_uvregression(
#'   trial[c("response", "age", "grade")],
#'   method = glm,
#'   y = response,
#'   method.args = list(family = binomial),
#'   exponentiate = TRUE
#' ) %>%
#' as_forest_plot()
#'
#' # Example 2 ------------------------------------
#' tbl <-
#'  coxph(Surv(ttdeath, death) ~ age + marker, trial) %>%
#'  tbl_regression(exponentiate = TRUE) %>%
#'  add_n()
#'
#' as_forest_plot(tbl, col_names = c("stat_n", "estimate", "ci", "p.value"))
#'
#' # Example 3 ----------------------------------
#' tbl %>%
#' modify_cols_merge(
#'   pattern = "{estimate} ({ci})",
#'   rows = !is.na(estimate)
#' ) %>%
#' modify_header(estimate = "HR (95% CI)") %>%
#' as_forest_plot(
#'   col_names = c("estimate", "p.value"),
#'   boxsize = 0.2,
#'   col = forestplot::fpColors(box = "darkred")
#' )
as_forest_plot <- function(x,
                           col_names = c("estimate", "ci", "p.value"),
                           graph.pos = 2,
                           boxsize = 0.3,
                           title_line_color = "darkblue",
                           xlog = x$inputs$exponentiate,
                           ...) {
  assert_package("forestplot", "as_forest_plot()")
  if (!inherits(x, c("tbl_regression", "tbl_uvregression"))) {
    stop("`x=` must be class 'tbl_regression' or 'tbl_uvregression'", call. = FALSE)
  }

  ###################################
  # Output the main text part table #
  ###################################
  txt_tb1 <-
    x %>%
    gtsummary::modify_column_unhide() %>%
    gtsummary::modify_fmt_fun(contains("stat") ~ gtsummary::style_number) %>%
    gtsummary::as_tibble(col_labels = FALSE)

  ############################################################
  # Prepare the header of the text part (only one line here) #
  ############################################################
  txt_tb2 <-
    x %>%
    gtsummary::modify_column_unhide() %>%
    gtsummary::as_tibble() %>%
    names()

  ### remove the gt bold sign "**" from the header
  ### And add a summary indicating variable
  txt_tb2 <-
    data.frame(matrix(gsub("\\**", "", txt_tb2), nrow = 1), stringsAsFactors = FALSE)
  names(txt_tb2) <- names(txt_tb1)

  txt_tb <- dplyr::bind_rows(txt_tb2, txt_tb1)

  #################################################
  # Combine the forest plot stats with the txt_tb #
  #################################################
  line_stats <-
    x$table_body %>%
    select(all_of(c("estimate", "conf.low", "conf.high"))) %>%
    dplyr::rename_with(~paste0(., "_num")) %>%
    tibble::add_row(.before = 0)

  ## form the forest plot input matrix ###
  forestplot_tb <-
    dplyr::bind_cols(txt_tb, line_stats) %>%
    mutate(..summary_row.. = dplyr::row_number() == 1L)

  ###############################################
  # Create forest plot and save necessary stats #
  ###############################################
  ### Create the labeltext input  ##
  label_txt <- forestplot_tb %>% select(all_of(c("label", col_names)))

  forestplot.obj <-
    forestplot_tb %>%
    forestplot::forestplot(
      mean = "estimate_num",
      lower = "conf.low_num",
      upper = "conf.high_num",
      graph.pos = graph.pos,
      lwd.zero = 2,
      boxsize = boxsize,
      labeltext = label_txt,
      graphwidth = grid::unit(9, "cm"),
      is.summary = "..summary_row..",
      hrzl_lines = list("2" = grid::gpar(lwd = 2, col = title_line_color)),
      xlog = xlog,
      ...
    )

  ## plot the forestplot if not saving ##
  forestplot.obj
}
