#' Likert Summary
#'
#' \lifecycle{experimental}
#' @inheritParams gtsummary::tbl_summary
#' @param statistic Formula or list of formulas specifying types of categorical
#' statistics to display, see [gtsummary::tbl_summary()] help page,
#' section *statistic argument*
#' @param digits Formula or list of formulas indicating how to display the
#' computed statistics, see [gtsummary::tbl_summary()] help page
#' @param sort Sort table based on mean scores? Must be one of
#' `c("default", "ascending", "descending")`
#' @param ... not used
#' @export
#' @name tbl_likert
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
#' tbl_likert(df) %>%
#'   add_n() %>%
#'   gtsummary::as_kable()
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

  if (is.character(statistic))
    statistic <- paste0("~\"", statistic, "\"") %>% as.formula()
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

#' Add column with N to a Likert table
#' @export
#' @inheritParams gtsummary::add_n.tbl_summary
#' @param x Object with class `tbl_likert` from the [tbl_likert()] function
add_n.tbl_likert <- function(x,
                             statistic = "{n}",
                             col_label = "**N**",
                             footnote = FALSE,
                             last = FALSE,
                             ...) {
  # check inputs ---------------------------------------------------------------
  if (!inherits(x, "tbl_likert"))
    stop("`x=` must be class 'tbl_likert'", call. = FALSE)

  data <- x$inputs$data
  include <- x$inputs$include

  if ("n" %in% names(x$table_body))
    stop("`add_n()` has already been applied", call. = FALSE)

  # compute stat ---------------------------------------------------------------
  data <-
    data %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(include),
      ~ as.numeric(.x)
    ))

  tbl_n <-
    gtsummary::tbl_summary(
      data,
      statistic = ~ "{length}", # at least one stat required
      include = include,
      type = ~ "continuous",
      missing = "no"
    ) %>%
    gtsummary::add_n(statistic = statistic, footnote = TRUE)

  df_n <-
    tbl_n$table_body %>%
    dplyr::select(dplyr::all_of(c("variable", "n")))

  footnote_content <-
    tbl_n$table_styling$footnote %>%
    dplyr::filter(.data$column == "n") %>%
    purrr::pluck("footnote")

  # merging new columns with `x$table_body` ------------------------------------
  x <- x %>%
    gtsummary::modify_table_body(
      ~dplyr::left_join(
        .x,
        df_n,
        by = "variable"
      ) %>%
        dplyr::relocate(
          .data$n,
          .after = ifelse(last, dplyr::last_col(), dplyr::all_of("label"))
        )
    ) %>%
    gtsummary::modify_table_styling(
      columns = .data$n,
      hide = FALSE,
      label = col_label
    )

  # Adding footnote if requested -----------------------------------------------
  if (footnote == TRUE) {
    x <- gtsummary::modify_footnote(x, "n" ~ footnote_content)
  }

  x

}

#' @export
add_continuous_stat <- function(x, ...) {
  UseMethod("add_continuous_stat")
}

#' Add continuous statistics to a Likert table
#'
#' This function converts Likert-scales into a numeric score and computes
#' continuous statistics based on this score.
#' @export
#' @param x Object with class `tbl_likert` from the [tbl_likert()] function
#' @param statistic String or formula indicating the statistic to be reported.
#' Default is the mean score. Other possible continuous statistics are described
#' in [gtsummary::tbl_summary()] help page, section *statistic argument*.
#' @param digits Formula or list of formulas indicating how to display the
#' computed statistics, see [gtsummary::tbl_summary()] help page
#' @param col_label String indicating the column label. Default is generated
#' from `statistic`.
#' @param footnote Logical argument indicating whether to print a footnote
#' clarifying the statistics presented. Default is `FALSE`
#' @param last Logical indicator to include the new column last in table.
#' Default is `TRUE`
#' @param score_values Vector indicating the numeric value of each factor level.
#' Default is `1:n` where `n` indicates the number of levels.
#' @param stat_col_name Optional string indicating the name of the new column
#' added to `x$table_body`
add_continuous_stat.tbl_likert <- function(x,
                                           statistic = "{mean}",
                                           digits = NULL,
                                           col_label = NULL,
                                           footnote = FALSE,
                                           last = TRUE,
                                           score_values = NULL,
                                           stat_col_name = NULL,
                                           ...) {
  # check inputs ---------------------------------------------------------------
  if (!inherits(x, "tbl_likert"))
    stop("`x=` must be class 'tbl_likert'", call. = FALSE)

  if (is.character(statistic))
    statistic <- paste0("~\"", statistic, "\"") %>% as.formula()

  data <- x$inputs$data
  include <- x$inputs$include
  likert_lvls <- levels(data[[include[1]]])

  if (is.null(score_values)) score_values <- 1:length(likert_lvls)
  if (!is.numeric(score_values))
    stop("`score_values=` must be numeric", call. = FALSE)
  if (length(score_values) != length(likert_lvls))
    stop(
      paste0("`score_values=` must be of length ", length(likert_lvls)),
      call. = FALSE
    )

  if (is.null(stat_col_name))
    stat_col_name <-
      select(x$table_body, dplyr::matches("^add_stat_\\d*[1-9]\\d*$")) %>%
      names() %>%
      length() %>%
      {
        paste0("add_stat_", . + 1)
      }

  if (stat_col_name %in% names(x$table_body))
    stop("`stat_col_name=` already exists in `x$table_body`", call. = FALSE)

  # compute stat ---------------------------------------------------------------
  data <-
    data %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(include),
      ~ as.numeric(score_values[as.integer(.x)])
    ))

  tbl_stat <-
    gtsummary::tbl_summary(
      data,
      statistic = statistic,
      digits = digits,
      include = include,
      type = ~ "continuous",
      missing = "no"
    )

  df_new_stat <-
    tbl_stat$table_body %>%
    dplyr::select(dplyr::all_of(c("variable", "stat_0")))
  names(df_new_stat) <- c("variable", stat_col_name)

  footnote_content <- tbl_stat$table_styling$footnote$footnote
  if (is.null(col_label))
    col_label <- paste0("**", footnote_content, "**")

  # merging new columns with `x$table_body` ------------------------------------
  x <- x %>%
    gtsummary::modify_table_body(
      ~dplyr::left_join(
        .x,
        df_new_stat,
        by = "variable"
      ) %>%
        dplyr::relocate(
          dplyr::all_of(stat_col_name),
          .after = ifelse(last, dplyr::last_col(), dplyr::all_of("label"))
        )
    ) %>%
    gtsummary::modify_table_styling(
      columns = dplyr::all_of(stat_col_name),
      hide = FALSE,
      label = col_label
    )

  # Adding footnote if requested -----------------------------------------------
  if (footnote == TRUE) {
    x <- gtsummary::modify_footnote(x, stat_col_name ~ footnote_content)
  }

  x
}
