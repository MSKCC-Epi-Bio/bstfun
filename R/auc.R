#' Calculate exact AUCs based on the distribution of risk in a population
#'
#' Provided a distribution of risk in a population, this function calculates the
#' exact AUC of a model that produces the risk estimates.  For example, a logistic
#' regression model built with a normal linear predictor yields logit-normal distributed
#' predicted risks.  The AUC from the logistic regression model is the same as the AUC estimated
#' from the distribution of the predicted risks, independent of the outcome.  This method for AUC
#' calculation is useful for simulation studies where the predicted risks are a mixture of two
#' distributions.  The exact prevalence of the outcome can easily be calculated, along with the exact
#' AUC of the model.
#'
#' @param x histogram object from [graphics::hist]
#' @param density a function name that describes the continuous probability density function of the
#' risk from 0 to 1.
#' @param cut.points sequence of points in \[0, 1\] where the sensitivity and specificity are calculated.
#' More points lead to a more precise estimate of the AUC.  Default is seq(from = 0, to = 1,by = 0.001).
#' @param ... arguments for the function specified in density.  For example, dbeta(x, shape1=1, shape2=1)
#' has need for two additional arguments to specify the density function (shape1 and shape2).
#' @importFrom stats integrate
#' @return Returns a list sensitivity and specificity at each cut point, the expected value or
#' mean risk, and the AUC associated with the distribution.
#'
#' @author Daniel D Sjoberg
#' @name auc
#' @examples
#' auc_density(density = dbeta, shape1 = 1, shape2 = 1)
#'
#' runif(10000) %>%
#'   hist(breaks = 250) %>%
#'   auc_histogram()
NULL

#' @export
#' @rdname auc
auc_density <- function(density, cut.points = seq(from = 0, to = 1, by = 0.001), ...) {
  # calculating distribution mean
  mu <- integrate(function(x) x * density(x, ...), 0, 1)$value

  sensitivity <- rep_len(NA_real_, length.out = length(cut.points))
  specificity <- rep_len(NA_real_, length.out = length(cut.points))
  for (i in seq_along(cut.points)) {
    c <- cut.points[i]
    # calculating Sens and Spec using Bayes Rule
    if (c %in% c(0, 1)) {
      sensitivity[i] <- dplyr::case_when(c %in% 0 ~ 1, c %in% 1 ~ 0)
      specificity[i] <- dplyr::case_when(c %in% 0 ~ 0, c %in% 1 ~ 1)
    }
    else {
      sensitivity[i] <- integrate(function(x) x * density(x, ...), c, 1)$value / mu
      specificity[i] <-
        (integrate(function(x) density(x, ...), 0, c)$value -
           integrate(function(x) x * density(x, ...), 0, c)$value) / (1 - mu)
    }
  }

  # calculating the AUC (using the trapezoidal rule)
  auc <- pracma::trapz(x = specificity, y = sensitivity)

  # returning results
  results <-
    list(
      details =
        tibble::tibble(
          threshold = cut.points,
          sensitivity = sensitivity,
          specificity = specificity
        ),
      mu = mu,
      auc = auc
    )

  return(results)
}

#' @export
#' @rdname auc
auc_histogram <- function(x) {

  # calculating mean
  mu <- stats::weighted.mean(x$mids, x$density)

  x_length <- length(x$count)
  x_width <- x$breaks[2:(x_length + 1)] - x$breaks[1:x_length]

  # calculating sensitivity and specificity
  sens_spec <-
    purrr::map_dfr(
      1:x_length,
      function(i) {
        # calculating Sens and Spec using Bayes Rule
        sens <- sum(x$mids[i:x_length] * x$density[i:x_length] * x_width[i:x_length]) / mu
        spec <- (sum(x$density[1:i] * x_width[1:i]) -
                   sum(x$mids[1:i] * x$density[1:i] * x_width[1:i])) / (1 - mu)
        tibble::tibble(x = x$mids[i], sensitivity = sens, specificity = spec)
      }
    )

  # calculating the AUC (using the trapezoidal rule)
  auc <- pracma::trapz(x = sens_spec$specificity, y = sens_spec$sensitivity)

  auc
}

