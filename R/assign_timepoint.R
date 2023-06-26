#' Assign a time point to a long data set with multiple measures
#'
#' Given a data set that has a measure collected over time and you want to extract,
#' for example the 3 month measurement, this function will find the measure
#' closest to 3 months within a defined window.
#'
#' @param data data frame
#' @param id id variable name, such as `"mrn"`
#' @param ref_date baseline or reference date column name
#' @param measure_date date the measure was collected
#' @param timepoints vector of time point to identify
#' @param windows list of windows around a time point that are acceptable
#' @param time_units one of `c("days", "weeks", "months", "years")`
#' @param new_var name of new variable, default is `"timepoint"`
#' @param keep_all_obs logical indicating whether to return a data frame
#' with only the assigned time points (default), or to return a data frame
#' with all rows.
#' @param keep_all_vars logical indicating whether to return a data frame
#' with all the variables in `data=` and the new time point column (default),
#' or a limited data frame including only the column involved in assigning
#' a time point.
#'
#' @importFrom lubridate %--%
#' @export
#' @return data frame passed in `data` with additional column `new_var`
#' @examples
#' ggplot2::economics_long %>%
#'   dplyr::group_by(variable) %>%
#'   dplyr::mutate(min_date = min(date)) %>%
#'   dplyr::ungroup() %>%
#'   assign_timepoint(
#'     id = variable,
#'     ref_date = min_date,
#'     measure_date = date,
#'     timepoints = c(6, 12, 24),
#'     windows = list(c(-2, 2), c(-2, 2), c(-2, 2)),
#'     time_units = "months"
#'   )
assign_timepoint <- function(data, id, ref_date, measure_date, timepoints, windows,
                             time_units = c("days", "weeks", "months", "years"),
                             new_var = "timepoint",
                             keep_all_obs = FALSE,
                             keep_all_vars = TRUE) {
  # checking inputs ------------------------------------------------------------
  stopifnot(is.data.frame(data))
  stopifnot(rlang::is_list(windows))
  stopifnot(is.numeric(timepoints))
  stopifnot(rlang::is_scalar_logical(keep_all_obs))
  stopifnot(rlang::is_scalar_logical(keep_all_vars))
  stopifnot(rlang::is_string(new_var))
  time_units <- match.arg(time_units)

  purrr::walk(
    windows,
    function(.x) {
      if (length(.x) != 2L)
        stop("Each element of list `windows=` must be a vector of length 2.", call. = FALSE)
      if (.x[1] >= .x[2])
        paste("Each element in `windows=` must be an interval where the",
              "first item is less than the second, e.g.",
              "`windows = list(c(-1, 1))`") %>%
        stringr::str_wrap() %>%
        stop(call. = FALSE)
      if (.x[1] > 0 || .x[2] < 0)
        paste("Each element in `windows=` must be an interval where the",
              "first item is less than the second, and the interval",
              "contains zero, e.g.",
              "`windows = list(c(-1, 1))`") %>%
        stringr::str_wrap() %>%
        stop(call. = FALSE)
      invisible()
    }
  )

  id <- broom.helpers::.select_to_varnames({{ id }}, data)
  ref_date <- broom.helpers::.select_to_varnames({{ ref_date }}, data, select_single = TRUE)
  measure_date <- broom.helpers::.select_to_varnames({{ measure_date }}, data, select_single = TRUE)

  stopifnot(lubridate::is.Date(data[[ref_date]]))
  stopifnot(lubridate::is.Date(data[[measure_date]]))
  if (new_var %in% names(data)) {
    stop("`new_var=` is already present in `data=` and cannot be added.")
  }

  # checking for duplicates within id and ref_date
  if (duplicated(data[c(id, measure_date)]) %>% sum() > 0) {
    warning("`data` is not unique within `id` and `measure_date`. Results may differ depending on order of `data`.")
  }

  # assigning a function to calculate difference between dates
  time_diff_fun <- switch(
    time_units,
    "days" = function(x, y) lubridate::as.duration(x %--% y) / lubridate::ddays(1),
    "weeks" = function(x, y) lubridate::as.duration(x %--% y) / lubridate::dweeks(1),
    "months" = function(x, y) lubridate::as.duration(x %--% y) / lubridate::dyears(1) * 12,
    "years" = function(x, y) lubridate::as.duration(x %--% y) / lubridate::dyears(1),
  )

  data[["..time_diff.."]] <- time_diff_fun(data[[ref_date]], data[[measure_date]])
  data[[new_var]] <- NA_real_

  # cycling through each timepoint and assigning an obs the timepoint
  for (i in seq_len(length(timepoints))) {
    data <-
      data %>%
      dplyr::group_by(!!!rlang::syms(id)) %>%
      dplyr::arrange(abs(.data$..time_diff.. - .env$timepoints[i])) %>%
      dplyr::mutate(
        "{new_var}" :=
          dplyr::if_else(
            dplyr::between(.data$..time_diff..,
                           .env$timepoints[i] + .env$windows[[i]][1],
                           .env$timepoints[i] + .env$windows[[i]][2]) &
              dplyr::row_number() == 1,
            .env$timepoints[i],
            .data[[new_var]]
          )
      )
  }

  # sorting data frame and ungrouping
  data <-
    data %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!!!rlang::syms(id), .data[["..time_diff.."]]) %>%
     dplyr::select(-"..time_diff..")

  # deleting obs that were not a selected timepoint
  if (!keep_all_obs) {
    data <-
      data %>%
      dplyr::filter(!is.na(!!rlang::sym(new_var)))
  }
  # only keeping vars for timepoint selection if indicated
  if (!keep_all_vars) {
    data <-
      data %>%
      dplyr::select(dplyr::all_of(c(id, ref_date, measure_date, new_var)))
  }

  return(data)
}
