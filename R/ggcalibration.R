#' Model Calibration Plot
#'
#' Assess a model's calibration via a calibration plot.
#'
#' @param data a data frame
#' @param y variable name of the outcome coded as 0/1
#' @param x variable name of the risk predictions
#' @param n.groups number of groups
#' @param conf.level level of confidence to be used in the confidence interval
#' @param methods method to use to construct the interval.
#' See [`binom::binom.confint()`] for details
#' @param geom_errorbar.args named list of arguments that will be passed
#' to `ggplot2::geom_errorbar()`. Default is `list(width = 0)`
#' @param geom_point.args named list of arguments that will be passed
#' to `ggplot2::geom_point()`. Default is `list()`
#'
#' @return ggplot
#' @export
#'
#' @examples
#' glm(response ~ age + marker + grade, trial, family = binomial) %>%
#'   broom::augment(type.predict = "response") %>%
#'   ggcalibration(y = response, x = .fitted, n.groups = 6)
ggcalibration <- function(data, y, x, n.groups = 10,  conf.level = 0.95,
                          methods = c("exact", "ac", "asymptotic", "wilson",
                                      "prop.test", "bayes", "logit", "cloglog", "probit"),
                          geom_errorbar.args = list(width = 0),
                          geom_point.args = list()) {
  rlang::check_installed("binom")
  methods <- match.arg(methods)
  # convert x,y inputs to character --------------------------------------------
  y <- dplyr::select(data, {{ y }}) %>% names()
  x <- dplyr::select(data, {{ x }}) %>% names()

  # split data and calculate rates ---------------------------------------------
  df_results <-
    dplyr::select(data, dplyr::all_of(c(y, x))) %>%
    tidyr::drop_na() %>%
    rlang::set_names(c("y", "x")) %>%
    dplyr::arrange(.data$x) %>%
    dplyr::mutate(
      group = (dplyr::row_number() - 1) %/% (dplyr::n() / n.groups) + 1L
    ) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::mutate(
      x.mean = mean(.data$x),
      y.mean = mean(.data$y),
      y.sum = sum(.data$y),
      y.length = length(.data$y)
    ) %>%
    dplyr::select(dplyr::all_of(c("group", "x.mean", "y.mean", "y.sum", "y.length"))) %>%
    dplyr::distinct() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      y.binom =
        tryCatch(
          suppressWarnings(
            binom::binom.confint(
              x = .data$y.sum,
              n = .data$y.length,
              conf.level = conf.level,
              methods = methods
            ) %>%
              dplyr::select(dplyr::all_of(c("lower", "upper"))) %>%
              rlang::set_names(c("y.conf.low", "y.conf.high"))
          ) ,
          error = function(e) NULL
        ) %>%
        list()
    ) %>%
    tidyr::unnest(.data$y.binom) %>%
    dplyr::ungroup()

  # plot results ---------------------------------------------------------------
  lst_gg_cmds <-
    rlang::list2(
      rlang::inject(ggplot2::geom_point(!!!geom_point.args)),
      rlang::inject(ggplot2::geom_errorbar(!!!geom_errorbar.args)),
      ggplot2::labs(y = attr(data[[y]], "label") %||% y,
                    x = attr(data[[x]], "label") %||% x)
    )

  df_results %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$x.mean, y = .data$y.mean,
                   ymin = .data$y.conf.low, ymax = .data$y.conf.high)
    ) +
    lst_gg_cmds
}


