#' Report Follow-up Among Censored Obs
#'
#' Function accepts `survival::Surv()` object, extracts the follow-up
#' times among the censored observations, and returns the requested
#' summary statistics. Use this function to report follow-up in-line in
#' R Markdown reports.
#'
#' @param Surv An object of class `"Surv"` created with `survival::Surv()`---not a multi-state endpoint.
#' @param data A data frame
#' @param pattern Statistics pattern to return. Default is
#' `"{median} (IQR {p25}, {p75})"`. User may select the following summary
#' statistics to report, `"{median}"`, `"{mean}"`, `"{p25}"`, `"{p75}"`,
#' `"{sd}"`, `"{var}"`, `"{min}"`, and `"{max}"`. Also available are
#' `"{n}"` (the number of events) and `"{N}"` (number of non-missing observations).
#' @param style_fun Function used to style/format the summary statistics.
#' Default is `gtsummary::style_sigfig`. Argument accepts anonymous function
#' notation, e.g. `~gtsummary::style_sigfig(., digits = 3)`
#'
#' @return string of summary statistics
#' @export
#'
#' @examples
#' library(survival)
#'
#' followup_time(Surv(time, status), data = lung)
#'
#' followup_time(
#'   Surv(time, status), data = lung,
#'   pattern = "{median} days",
#'   style_fun = ~gtsummary::style_sigfig(., digits = 4)
#' )
#'
#' followup_time(
#'   survival::Surv(time, status),
#'   data = survival::lung,
#'   pattern = "{n} events with a follow-up time of {median} days (IQR {p25}, {p75})"
#' )


followup_time <- function(Surv, data = NULL,
                          pattern = "{median} (IQR {p25}, {p75})",
                          style_fun = gtsummary::style_sigfig) {
  # check inputs ---------------------------------------------------------------
  Surv <-
    tryCatch(
      rlang::eval_tidy(rlang::enquo(Surv), data = data),
      error = function(e) {
        cli::cli_alert_danger("There was an error evaluating {.code Surv=}")
        e
      }
    )
  if (!inherits(Surv, "Surv")) {
    stop("`Surv=` must be class 'Surv' created with `survival::Surv()`")
  }

  # evaluate style_fun ---------------------------------------------------------
  style_fun <- purrr::as_mapper(style_fun)

  # remove rows with NA --------------------------------------------------------
  Surv <- Surv[stats::complete.cases(Surv), ]

  # extract followup time among censored patients ------------------------------
  censored_follow_up <- Surv[Surv[, 2] %in% 0L, 1]

  # save summary stats in list -------------------------------------------------
  follow_up_stats <-
    list(
      median = stats::median(censored_follow_up),
      mean = mean(censored_follow_up),
      p25 = stats::quantile(censored_follow_up, probs = 0.25),
      p75 = stats::quantile(censored_follow_up, probs = 0.75),
      sd = stats::sd(censored_follow_up),
      var = stats::var(censored_follow_up),
      min = min(censored_follow_up),
      max = max(censored_follow_up),
      n = sum(Surv[, 2]),
      N = nrow(Surv)
    ) %>%
    purrr::imap(
      ~ifelse(
        .y %in% c("n", "N"),
        gtsummary::style_number(.x),
        style_fun(.x)
      )
    )

  # return summary stats -------------------------------------------------------
  glue::glue_data(.x = follow_up_stats, pattern)
}
