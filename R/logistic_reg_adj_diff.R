#' Logistic regression adjusted differences
#'
#' This function works with `gtsummary::add_difference()` to calculate
#' adjusted differences and confidence intervals based on results from a
#' logistic regression model. The function uses bootstrap methods to build
#' regression model and estimate the adjusted difference between two groups.
#' The CI is estimate by either using the SD from the bootstrap difference
#' estimates and calculating the CI assuming normality or using the centiles
#' of the bootstrapped differences as the confidence limits
#'
#' @param data a data frame
#' @param variable string of binary variable in `data=`
#' @param by string of the `by=` variable name
#' @param adj.vars character vector of variable names to adjust model for
#' @param type string indicating the summary type
#' @param ci_type string dictation bootstrap method for CI estimation.
#' Must be one of `c("sd", "centile")`.
#' @param boot_n number of bootstrap iterations to use. In most cases, it is
#' reasonable to used 250 for the `"sd"` method and 5000 for the `"centile"`
#' method.
#' @param ... not used
#' @inheritParams gtsummary::add_difference
#'
#' @return tibble with difference estimate
#' @family gtsummary-related functions
#' @export
#'
#' @examplesIf FALSE
#' tbl <- tbl_summary(trial, by = trt, include  = response, missing = "no")
#'
#' # Example 1 -----------------------------------------------------------------
#' logistic_reg_adj_diff_ex1 <-
#'   tbl %>%
#'   add_difference(
#'     test = everything() ~ logistic_reg_adj_diff,
#'     adj.vars = "stage"
#'   )
#'
#' # Example 2 -----------------------------------------------------------------
#' # Use the centile method, and
#' # change the number of bootstrap resamples to perform
#' logistic_reg_adj_diff_ex2 <-
#'   tbl %>%
#'   add_difference(
#'     test = everything() ~
#'       purrr::partial(logistic_reg_adj_diff, ci_type = "centile", boot_n = 100),
#'     adj.vars = "stage"
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{logistic_reg_adj_diff_ex1.png}{options: width=80\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{logistic_reg_adj_diff_ex2.png}{options: width=80\%}}
logistic_reg_adj_diff <- function(data, variable, by, adj.vars, conf.level, type,
                                  ci_type = c("sd", "centile"),
                                  boot_n = 250, ...) {
  # check inputs ---------------------------------------------------------------
  if (!type %in% "dichotomous") stop("Method only uses type 'dichotomous'.")
  ci_type <- match.arg(ci_type)
  if (length(data[[by]] %>% stats::na.omit() %>% unique()) != 2) {
    stop("`by=` must have exactly 2 levels", call. = FALSE)
  }

  # remove missing values
  data <- data[c(variable, by, adj.vars)] %>% dplyr::filter(stats::complete.cases(.))

  # calculate central difference estimate --------------------------------------
  central_estimate <-
    glm_diff(data = data, variable = variable, by = by, adj.vars = adj.vars)

  # calculate vector of bootstrp difference estimates --------------------------
  bootstrapped_estimates <-
    seq_len(boot_n) %>%
    purrr::map_dbl(
      function(i) {
        tryCatch(
          glm_diff(data = data[sample(nrow(data), nrow(data), replace = TRUE), ],
                   variable = variable, by = by, adj.vars = adj.vars),
          error = function(e) NA
        )
      }
    ) %>%
    purrr::discard(is.na)

  # estimate the CI ------------------------------------------------------------
  if (ci_type %in% "sd") {
    df_result <-
      tibble::tibble(
        estimate = central_estimate,
        conf.low = central_estimate + stats::qnorm((1 - conf.level) / 2) * stats::sd(bootstrapped_estimates),
        conf.high = central_estimate - stats::qnorm((1 - conf.level) / 2) * stats::sd(bootstrapped_estimates)
      )
  }
  else if (ci_type %in% "centile") {
    df_result <-
      tibble::tibble(
        estimate = central_estimate,
        conf.low = stats::quantile(bootstrapped_estimates, probs = (1 - conf.level) / 2),
        conf.high = stats::quantile(bootstrapped_estimates, probs = 1 - (1 - conf.level) / 2)
      )
  }

  # return results -------------------------------------------------------------
  df_result %>%
    dplyr::mutate(method = "Logistic regression adjusted bootstrapped difference")
}

# function to build logistic regression model, and return difference between groups
glm_diff <- function(data, variable, by, adj.vars) {
  model.matrix <-
    model.matrix(
      stringr::str_glue("{variable} ~  .") %>% stats::as.formula(),
      data = data
    )[, -1, drop = FALSE]

  mod <- stats::glm(factor(data[[variable]]) ~ model.matrix, family = stats::binomial)

  newdata <- prep_new_data(model.matrix)
  preds <- (newdata %*% stats::coef(mod)[-1] + stats::coef(mod)[1]) %>% inverse_logit()

  c(preds) %>% purrr::reduce(`-`)
}

inverse_logit <- function(x) {
  exp(x) / (1 + exp(x))
}

# function to make a new data matrix, the mean values in the adjusted var cols
prep_new_data <- function(model.matrix) {
  model.matrix[!duplicated(model.matrix[, 1, drop = FALSE]), 1, drop = FALSE] %>%
    {.[order(.[,1], decreasing = FALSE),]} %>%
    cbind(
      colMeans(model.matrix[, -1, drop = FALSE]) %>%
        as.matrix() %>%
        t() %>%
        rep_row(2)
    )
}
rep_row <- function(x, n){
  res <- matrix(rep(x,each=n),nrow=n)
  if (!is.null(colnames(x))) colnames(res) <- colnames(x)
  res
}



