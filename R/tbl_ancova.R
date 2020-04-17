#' Table difference between two groups
#'
#' Returns an ANCOVA table showing the means of two groups and the
#' difference in means
#' @param data frame to be used in ANCOVA models
#' @param y vector of continuous outcome variables. One-way ANOVA/ANCOVA models
#' will be computed for each outcome.
#' @param x string indicating the binary comparison variable
#' @inheritParams gtsummary::tbl_uvregression
#' @inheritParams gtsummary::tbl_summary
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' tbl_ancova_ex1 <-
#'   trial %>%
#'   tbl_ancova(y = c("age", "marker"), x = "trt")
#' @section Example Output:
#' \if{html}{\figure{tbl_ancova_ex1.png}{options: width=80\%}}

tbl_ancova <- function(data, y, x, formula = "{y} ~ {x}", label = NULL,
                       method.args = NULL, conf.level = 0.95,
                       estimate_fun = NULL, pvalue_fun = NULL,
                       method = stats::lm, digits = NULL) {
  # converting inputs to strings/lists
  y <- dplyr::select(data[0, , drop = FALSE], {{ y }}) %>% names()
  x <- dplyr::select(data[0, , drop = FALSE], {{ x }}) %>% names()
  label <- tidyselect_to_list(data, label)
  digits <- tidyselect_to_list(data, digits)

  # will return call, and all object passed to in tbl_regression call
  # the object func_inputs is a list of every object passed to the function
  func_inputs <- as.list(environment())

  # checking the x variable has two levels
  if (data[[x]] %>% stats::na.omit() %>% unique() %>% length() != 2) {
    stop(glue::glue("The stratifying variable, '{x}', must have two levels."))
  }

  # building models ------------------------------------------------------------
  df_ancova <-
    tibble::tibble(y = y) %>%
    dplyr::mutate(
      label = map_chr(.data$y, ~ label[[.x]] %||% attr(data[[.x]], "label") %||% .x),
      # building formula list
      formula = map(.data$y, function(y) glue::glue(.env$formula) %>% stats::as.formula()),
      # building models
      models =
        map(.data$formula, ~ do.call(method, list(data = data, formula = .x, method.args))),
      # formatting model
      tbl_regression = pmap(
        list(.data$models, .data$y, .data$label),
        ~ gtsummary::tbl_regression(
          x = ..1, conf.level = conf.level,
          show_single_row = .env$x,
          estimate_fun = estimate_fun, pvalue_fun = pvalue_fun,
          label = glue("{.env$x} ~ '{..3}'") %>% as.formula(),
          include = .env$x
        )
      ),
      tbl_regression = map2(
        .data$tbl_regression, .data$y,
        function(x, y) {
          x$table_body$variable = y
          x
        }
      )
    )

  # appending all tbl_regressions together
  gts_ancova <-
    gtsummary::tbl_stack(df_ancova$tbl_regression) %>%
      gtsummary::modify_header(estimate = "**Difference**")


  # building summary statistics ------------------------------------------------
  # list of variable adjusted for
  vars_adjust <-
    formula %>%
    str_replace(fixed("{y}"), fixed(".")) %>%
    str_replace(fixed("{x}"), fixed(".")) %>%
    stats::as.formula() %>%
    all.vars() %>%
    purrr::discard(~ . == ".")


  df_summary <-
    df_ancova %>%
    select(c("y", "label")) %>%
    mutate(
      tbl_summary = map2(
        .data$y, .data$label,
        ~ data %>%
          select(c(.x, .env$x, vars_adjust)) %>%
          na.omit() %>%
          mutate(!!x := as_factor(.data[[!!x]]) %>% fct_rev()) %>%
          select(c(.x, .env$x)) %>%
          gtsummary::tbl_summary(
            by = .env$x,
            label = glue::glue("everything() ~ '{.y}'") %>% as.formula(),
            type = everything() ~ "continuous",
            statistic = everything() ~ "{mean} ({sd})",
            digits = digits
          ) %>%
          gtsummary::modify_header(stat_by = "**{level}**") %>%
          gtsummary::add_n()
      )
    )
  # appending all tbl_summaries together
  gts_summary <- gtsummary::tbl_stack(df_summary$tbl_summary)

  # merging summary stats and ancova results -----------------------------------
  result <- gtsummary::tbl_merge(list(gts_summary, gts_ancova))

  # removing spanning header ---------------------------------------------------
  result$table_header$spanning_header <- NA_character_

  class(result) <- c("tbl_ancova", "gtsummary")
  result
}
