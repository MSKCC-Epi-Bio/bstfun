#' Likert Summary Table
#'
#' \lifecycle{experimental}
#' Creates a summary of Likert scales following the gtsummary structure
#'
#' @inheritParams gtsummary::tbl_summary
#' @param sort Sort table based on mean scores? Must be one of
#' `c("default", "ascending", "descending")`
#' @param x Table of class 'tbl_likert'
#' @param ... not used
#' @family gtsummary-related functions
#' @export
#' @examples
#' df <-
#'   tibble::tibble(
#'     f1 =
#'       sample.int(100, n = 3, replace = TRUE) %>%
#'       factor(levels = 1:3, labels = c("bad", "meh", "good")),
#'     f2 =
#'       sample.int(100, n = 3, replace = TRUE) %>%
#'       factor(levels = 1:3, labels = c("bad", "meh", "good")),
#'   )
#'
#' tbl_likert_ex1 <-
#'   tbl_likert(df) %>%
#'   add_n()
#' @export
#' @name tbl_likert
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_likert_ex1.png}{options: width=50\%}}
NULL

#' @export
#' @rdname tbl_likert
tbl_likert <- function(data,
                       label = NULL, statistic = NULL, digits = NULL,
                       include = everything(),
                       sort = c("default", "ascending", "descending")) {
  # evaluate inputs ------------------------------------------------------------
  sort <- match.arg(sort)
  include <-
    broom.helpers::.select_to_varnames( {{ include }},
                                        data = data, arg_name = "include")
  label <-
    broom.helpers::.formula_list_to_named_list(
      label,
      data = data,
      arg_name = "label"
    )
  statistic <-
    broom.helpers::.formula_list_to_named_list(
      statistic,
      data = data,
      arg_name = "statistic"
    )
  digits <-
    broom.helpers::.formula_list_to_named_list(
      digits,
      data = data,
      arg_name = "digits"
    )
  # saving function inputs
  func_inputs <- as.list(environment())

  # all variables in data must be a factor with the same levels ----------------
  data_lbls <- map(data, ~attr(.x, "label"))
  data <-
    data[include] %>%
    mutate(
      dplyr::across(
        .cols = everything(),
        function(.x) {
          if (inherits(.x, "factor")) return(.x)
          factor(.x)
        }
      )
    ) %>%
    forcats::fct_unify() %>%
    dplyr::bind_cols()

  # create summary table, then stack -------------------------------------------
  result <-
    names(data) %>%
    purrr::map(
      ~ gtsummary::tbl_summary(
          data =
            data %>%
            dplyr::mutate(...all_true... = TRUE) %>%
            tidyr::drop_na(all_of(.x)),
          by = all_of(.x),
          include = ...all_true...,
          type = list(...all_true... = "dichotomous"),
          statistic = list(...all_true... = statistic[[.x]]),
          percent = "row",
          digits = list(...all_true... = digits[[.x]]),
          label = list(...all_true... = (label[[.x]] %||% data_lbls[[.x]] %||% .x))
        ) %>%
        gtsummary::modify_header(gtsummary::all_stat_cols() ~ "**{level}**") %>%
        gtsummary::tbl_butcher() %>%
        gtsummary::modify_table_body(
          function(table_body) {
            table_body %>% dplyr::mutate(variable = .x)
          }
        )
    ) %>%
    gtsummary::tbl_stack(quiet = TRUE) %>%
    # add function inputs to returned list list
    purrr::list_modify(inputs = func_inputs)

  # sorting if needed ----------------------------------------------------------
  if (sort %in% c("ascending", "descending")) {
    df_means <-
      data %>%
      purrr::imap_dbl(~as.numeric(.) %>% mean(na.rm = TRUE)) %>%
      tibble::enframe("variable", "score_mean")
    df_means <-
      switch (
        sort,
        "ascending" = dplyr::arrange(df_means, .data$score_mean),
        "descending" = dplyr::arrange(df_means, dplyr::desc(.data$score_mean))
      )

    result <-
      result %>%
      gtsummary::modify_table_body(
        ~dplyr::left_join(
          df_means %>% select(.data$variable),
          .x,
          by = "variable"
        )
      )
  }

  # return tbl -----------------------------------------------------------------
  class(result) <- c("tbl_likert", "gtsummary")
  result
}


#' @export
#' @rdname tbl_likert
add_n.tbl_likert <- function(x, ...) {
  # check inputs ---------------------------------------------------------------
  if (!inherits(x, "tbl_likert"))
    stop("`x=` must be class 'tbl_likert'", call. = FALSE)

  # add n column ---------------------------------------------------------------
  df_n <-
    x$inputs$include %>%
    purrr::map_dfr(
      ~tibble::tibble(
        variable = .x,
        n = sum(!is.na(x$inputs$data[[.x]]))
      )
    )

  x %>%
    gtsummary::modify_table_body(
      ~dplyr::left_join(
        .x,
        df_n,
        by = "variable"
      ) %>%
        dplyr::relocate(.data$n, .after = .data$label)
    ) %>%
    gtsummary::modify_table_styling(
      columns = .data$n,
      hide = FALSE,
      label = "**N**",
      fmt_fun = gtsummary::style_number
    )
}
