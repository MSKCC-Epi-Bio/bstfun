#' Calculate eGFR
#'
#' @param creatinine serum creatinine level in mg/dL
#' @param age patient age
#' @param female logical indicating whether patient is female
#' @param aa logical indicating whether patient is African-American
#' @param label label that will be applied to result,
#' e.g. `attr("label", 'eGFR, mL/min/1.73m\U00B2')`
#'
#' @rdname egfr
#' @return numeric vector
#' @name egfr
#'
#' @examples
#' egfr_mdrd(creatinine = 1.2, age = 60, female = TRUE, aa = TRUE)
#' egfr_ckdepi(creatinine = 1.2, age = 60, female = TRUE, aa = TRUE)
NULL

#' @export
#' @rdname egfr
egfr_ckdepi <- function(creatinine, age, female, aa,
                      label = "eGFR, mL/min/1.73m\U00B2") {
  if (!is.numeric(creatinine) || !is.numeric(age) ||
      !is.logical(female) || !is.logical(aa)) {
    stop("Arguments `creatinine` and `age` must be numeric, and `female` and `aa` logical.")
  }

  kappa <- dplyr::case_when(female ~ 0.7, !female ~ 0.9)
  alpha <- dplyr::case_when(female ~ -0.329, !female ~ -0.411)
  female_constant <- dplyr::case_when(female ~ 1.018, !female ~ 1)
  black_constant <- dplyr::case_when(aa ~ 1.159, !aa ~ 1)

  egfr <-
    141 * pmin(creatinine / kappa, 1L) ^ alpha *
    max(creatinine / kappa, 1L) ^ -1.209 *
    0.993 ^ age * female_constant * black_constant

  attr(egfr, "label") <- label
  egfr
}

#' @export
#' @rdname egfr
egfr_mdrd <- function(creatinine, age, female, aa,
                      label = "eGFR, mL/min/1.73m\U00B2") {
  if (!is.numeric(creatinine) || !is.numeric(age) ||
      !is.logical(female) || !is.logical(aa)) {
    stop("Arguments `creatinine` and `age` must be numeric, and `female` and `aa` logical.")
  }

  female_constant <- dplyr::case_when(female ~ 0.742, !female ~ 1)
  black_constant <- dplyr::case_when(aa ~ 1.212, !aa ~ 1)


  egfr <-
    175 * creatinine^-1.154 * age^-0.203 * female_constant * black_constant
  attr(egfr, "label") <- label
  egfr
}
