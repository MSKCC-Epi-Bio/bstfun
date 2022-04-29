#' Deprecated functions
#'
#' \lifecycle{deprecated}
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

# v0.2.5 (2020-10-16) ----------------------------------------------------------
#' @rdname deprecated
#' @export
tbl_2way_summary <- function(data, row, col, con, label = NULL,
                             statistic = "{median} ({p25}, {p75})") {
  lifecycle::deprecate_warn("0.2.5", "bstfun::tbl_2way_summary()",
                            "gtsummary::tbl_continuous()")

  gtsummary::tbl_continuous(
    data = data,
    variable = {{ con }},
    by = {{ col }},
    include = {{ row }},
    statistic = everything() ~ statistic,
    label = everything() ~ label
  )
}

#' @rdname deprecated
#' @export
gtsummary_butcher <- function(x) {
  lifecycle::deprecate_warn(
    when = "0.2.5", what = "bstfun::gtsummary_butcher()",
    with = "gtsummary::tbl_butcher()")

  gtsummary::tbl_butcher(x)
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
  lifecycle::deprecate_warn("0.1.5", "bstfun::tbl_ancova()", "gtsummary::add_difference()")

  # converting inputs to strings/lists
  y <- dplyr::select(data[0, , drop = FALSE], {{ y }}) %>% names()
  x <- dplyr::select(data[0, , drop = FALSE], {{ x }}) %>% names()
  label <-
    broom.helpers::.formula_list_to_named_list(
      {{ label }},
      data = data,
      arg_name = "label"
    )
  digits <-
    broom.helpers::.formula_list_to_named_list(
      {{ digits }},
      data = data,
      arg_name = "digits"
    )

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
          gtsummary::modify_header(gtsummary::all_stat_cols() ~ "**{level}**") %>%
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
