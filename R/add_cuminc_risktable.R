#' Add risk table to `cuminc()` plot
#'
#' Plot cumulative incidence estimates with a risk table and estimates below
#' the figure.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' See `ggsurvfit::ggcuminc()` for improved options for reporting cumulative
#' incidence estimates (e.g. `add_quantile(x_value)`).
#'
#' @param cuminc `cmprsk::cuminc()` object
#' @param survfit `survival::survfit()` object
#' @param timepts a numeric vector of time points of the estimates to display.
#' E.g. c(0,1,2,3,4,5) or seq(0,12,by=2)
#' @param lg legend label for each cumulative incidence curve to be displayed.
#' E.g. c("Male", "Female")
#' @param numgrps the number of groups of the stratification variable: 1 is
#' no stratification, can stratify up to 3 groups
#' @param line to adjust position of risk table. A lower value will shift
#' table up, a larger value will shift table down; default is 3
#' @param at to adjust position of left margin. Default is -1.
#' @param col.list list of colors for legend text Should match the colors
#' of plot legend. Default is 1 (black).
#'
#' @author Meier Hsu
#' @export
#' @examplesIf interactive()
#' library(cmprsk)
#' library(survival)
#'
#' data(pbc)
#' # recode time
#' pbc$time.y <- pbc$time / 365.25
#' # recode status- switch competing events
#' pbc$status2 <- ifelse(pbc$status > 0, ifelse(pbc$status == 1, 2, 1), 0)
#' # recode stage for 3 groups
#' pbc$stage.3g <- ifelse(pbc$stage %in% c(1,2), "1-2", as.character(pbc$stage))
#'
#'
#' # Example 1 -------------------------------------
#' # CIR and KM for no strata
#' cif1 <- cuminc(ftime = pbc$time.y, fstatus = pbc$status2)
#' km1 <- survfit(Surv(pbc$time.y, pbc$status2 == 1) ~ 1)
#'
#' # Plot and add risk table for no strata (numgrps=1)
#' windows(5, 5)
#' par(mfrow = c(1, 1),
#'     mar = c(12.5, 5.7, 2, 2),
#'     mgp = c(2, 0.65, 0))
#' plot(cif1,
#'      curvlab = c("recurred", "died"),
#'      xlim = c(0, 12), xaxt = "n")
#' axis(1, at = seq(0, 12, 3))
#'
#' add_cuminc_risktable(cif1, km1,
#'                      timepts = seq(0, 12, 3),
#'                      lg = "",
#'                      numgrps = 1)
#'
#' # Example 2 -------------------------------------
#' cif2 <- cuminc(ftime = pbc$time.y,
#'                fstatus = pbc$status2,
#'                group = pbc$sex)
#' km2 <- survfit(Surv(pbc$time.y, pbc$status2 == 1) ~ pbc$sex)
#'
#' # Plot and add risk table for 2 groups (numgrps=2)
#' windows(5, 5)
#' par(mfrow = c(1, 1),
#'     mar = c(12.5, 5.7, 2, 2),
#'     mgp = c(2, 0.65, 0))
#'
#' plot(cif2,
#'      curvlab = c("male", "female", "", ""),
#'      lty = c(1, 2, 0, 0),
#'      xlim = c(0, 12),
#'      xaxt = "n", col = c(1, 2, 0, 0))
#' axis(1, at = seq(0, 12, 3))
#'
#' add_cuminc_risktable(cif2, km2,
#'                      timepts = seq(0, 12, 3),
#'                      lg = c("male", "female"),
#'                      numgrps = 2, col.list = c(1,2))
#'
#' # Example 3 -------------------------------------
#' cif3 <- cuminc(ftime = pbc$time.y,
#'                fstatus = pbc$status2,
#'                group = pbc$stage.3g)
#' km3 <- survfit(Surv(pbc$time.y, pbc$status2 == 1) ~ pbc$stage.3g)
#' windows(6,6)
#' par(mfrow = c(1, 1),
#'     mar = c(14, 5.7, 2, 2), # change bottom margin
#'     mgp = c(2, 0.65, 0))
#'
#' plot(cif3,
#'      curvlab = c("1-2", "3", "4", rep("",3)),
#'      lty = c(1, 2, 3, rep(0, 3)),
#'      xlim = c(0, 12),
#'      xaxt = "n", col = c(1, 2, 4, rep(0, 3)))
#' axis(1, at = seq(0, 12, 3))
#'
#' add_cuminc_risktable(cif3, survfit = km3,
#'                      timepts = seq(0, 12, 3),
#'                      lg = c("1-2", "3", "4"),
#'                      numgrps = 3, col.list = c(1,2,4))



add_cuminc_risktable <- function(cuminc, survfit, timepts, lg, numgrps,
                                 line = 3, at = -1,
                                 col.list = 1) {

  lifecycle::deprecate_soft("0.5.2", "add_cuminc_risktable()",
                            details = "See `ggsurvfit::ggcuminc()` for improved options for reporting cumulative incidence estimates (e.g. `add_quantile(x_value)`)")
  assert_package("cmprsk", "add_cuminc_risktable()")

  # CIR estimates
  CIR <- cmprsk::timepoints(cuminc, timepts)
  CIR1 <- round((CIR$est[1, ]) * 100, 1)

  # number at risk from Kaplan-Meier
  aa1 <- summary(survfit, times = timepts)

  # critical value for confidence interval
  z <- stats::qnorm(1 - (1 - .95) / 2)

  if (numgrps == 1) {
    # reformat data for number at risk
    nrisk <- cbind(aa1$n.risk, aa1$time)

    # confidence interval for 1 group
    confint1 <-
      c("(-,-)",
        paste0(
          "(",
          round(CIR$est[1, -1]^exp(-z * sqrt(CIR$var[1, -1]) / (CIR$est[1, -1] * log(CIR$est[1, -1]))), 2) * 100,
          ",",
          round(CIR$est[1, -1]^exp(z * sqrt(CIR$var[1, -1]) / (CIR$est[1, -1] * log(CIR$est[1, -1]))), 2) * 100,
          ")"
        )
      )

    # graphics::mtext("No. At Risk:", side = 1, line = 3, at = at, adj = 1, cex = 0.9, font = 2)
    graphics::mtext(paste0(lg[1],', n:'), side = 1, line = line, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[1])
    graphics::mtext("CIR (%):", side = 1, line = line + 1, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[1])
    graphics::mtext("95% CI:", side = 1, line = line + 2, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[1])

    ### NUMBER AT RISK FOR FIRST SUBGROUP AFTER TIME 0
    for (i in 1:length(timepts)) {
      nindex <- which(nrisk[, 2] == timepts[i]) # the ith timepoint
      graphics::mtext(nrisk[nindex[1], 1], side = 1, line = line, at = timepts[i], cex = .9, col = col.list[1])
    }

    ### CIR Estimates
    for (i in 1:length(timepts)) {
      graphics::mtext(CIR1[i], side = 1, line = line + 1, at = timepts[i], cex = 1, col = col.list[1])
    }

    ### 95% CI
    for (i in 1:length(timepts)) {
      graphics::mtext(confint1[i], side = 1, line = line + 2, at = timepts[i], cex = 0.95, col = col.list[1])
    }
  }


  # ----------------------------------------------------
  # function to add numbers at risk and legend, for 2 strata
  if (numgrps == 2) {
    # number at Risk for 2 groups
    nrisk <- cbind(aa1$strata, aa1$n.risk, aa1$time)
    # make wide dataset by time
    nriskmat <- tidyr::spread(as_tibble(nrisk), key = "V1", value = "V2")
    # nriskmat = as_tibble(nrisk) %>%
    #   tidyr::spread(V1,V2)
    # replace NA with 0, if needed
    nriskmat[is.na(nriskmat)] <- 0
    # make long dataset stacking by group
    nrisklong <- tidyr::gather(nriskmat, value = "nrisk", key = "group", "1":"2")


    # Estimates for 2 group
    CIR1 <- round((CIR$est[1, ]) * 100)
    CIR2 <- round((CIR$est[2, ]) * 100)

    # confidence interval for 2 groups
    confint1 <-
      c("(-,-)",
        paste0(
          "(",
          round(CIR$est[1, -1]^exp(-z * sqrt(CIR$var[1, -1]) / (CIR$est[1, -1] * log(CIR$est[1, -1]))), 2) * 100,
          ",",
          round(CIR$est[1, -1]^exp(z * sqrt(CIR$var[1, -1]) / (CIR$est[1, -1] * log(CIR$est[1, -1]))), 2) * 100,
          ")"
        ))
    confint2 <- c("(-,-)", paste0(
      "(",
      round(CIR$est[2, -1]^exp(-z * sqrt(CIR$var[2, -1]) / (CIR$est[2, -1] * log(CIR$est[2, -1]))), 2) * 100,
      ",",
      round(CIR$est[2, -1]^exp(z * sqrt(CIR$var[2, -1]) / (CIR$est[2, -1] * log(CIR$est[2, -1]))), 2) * 100,
      ")"
    ))

    # graphics::mtext("No. At Risk:", side = 1, line = 3, at = at, adj = 1, cex = 0.9, font = 2)
    graphics::mtext(paste0(lg[1],', n:'), side = 1, line = line, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[1])
    graphics::mtext("CIR (%):", side = 1, line = line + 1, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[1])
    graphics::mtext("95% CI:", side = 1, line = line + 2, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[1])

    graphics::mtext(paste0(lg[2],', n:'), side = 1, line = line + 3, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[2])
    graphics::mtext("CIR (%):", side = 1, line = line + 4, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[2])
    graphics::mtext("95% CI:", side = 1, line = line + 5, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[2])


    ### Number at risk both groups
    for (i in 1:length(timepts)) {
      nindex <- which(nrisklong[, 1] == timepts[i])
      graphics::mtext(nrisklong[nindex[1], 3], side = 1, line = line, at = timepts[i], cex = .9, col = col.list[1])
      graphics::mtext(nrisklong[nindex[2], 3], side = 1, line = line + 3, at = timepts[i], cex = .9, col = col.list[2])
    }

    ### Estimates for group 1
    for (i in 1:length(timepts)) {
      graphics::mtext(CIR1[i], side = 1, line = line + 1, at = timepts[i], cex = 1, col = col.list[1])
    }
    ### 95% CI for  group 1
    for (i in 1:length(timepts)) {
      graphics::mtext(confint1[i], side = 1, line = line + 2, at = timepts[i], cex = .95, col = col.list[1])
    }

    # Estimates for group 2
    for (i in 1:length(timepts)) {
      graphics::mtext(CIR2[i], side = 1, line = line + 4, at = timepts[i], cex = 1, col = col.list[2])
    }

    # 95% CI for  group 2
    for (i in 1:length(timepts)) {
      graphics::mtext(confint2[i], side = 1, line = line + 5, at = timepts[i], cex = .95, col = col.list[2])
    }
  }

  # ----------------------------------------------------
  # function to add numbers at risk and legend, for 3 strata
  if (numgrps == 3) {
    # number at Risk for 3 groups
    nrisk <- cbind(aa1$strata, aa1$n.risk, aa1$time)
    # make wide dataset by time
    nriskmat <- tidyr::spread(as_tibble(nrisk), key = "V1", value = "V2")
    # nriskmat = as_tibble(nrisk) %>%
    #   tidyr::spread(V1,V2)
    # replace NA with 0, if needed
    nriskmat[is.na(nriskmat)] <- 0
    # make long dataset stacking by group
    nrisklong <- tidyr::gather(nriskmat, value = "nrisk", key = "group", "1":"3")


    # Estimates for 3 group
    CIR1 <- round((CIR$est[1, ]) * 100)
    CIR2 <- round((CIR$est[2, ]) * 100)
    CIR3 <- round((CIR$est[3, ]) * 100)

    # confidence interval for 2 groups
    confint1 <-
      c("(-,-)",
        paste0(
          "(",
          round(CIR$est[1, -1]^exp(-z * sqrt(CIR$var[1, -1]) / (CIR$est[1, -1] * log(CIR$est[1, -1]))), 2) * 100,
          ",",
          round(CIR$est[1, -1]^exp(z * sqrt(CIR$var[1, -1]) / (CIR$est[1, -1] * log(CIR$est[1, -1]))), 2) * 100,
          ")"
        ))
    confint2 <- c("(-,-)", paste0(
      "(",
      round(CIR$est[2, -1]^exp(-z * sqrt(CIR$var[2, -1]) / (CIR$est[2, -1] * log(CIR$est[2, -1]))), 2) * 100,
      ",",
      round(CIR$est[2, -1]^exp(z * sqrt(CIR$var[2, -1]) / (CIR$est[2, -1] * log(CIR$est[2, -1]))), 2) * 100,
      ")"
    ))
    confint3 <- c("(-,-)", paste0(
      "(",
      round(CIR$est[3, -1]^exp(-z * sqrt(CIR$var[3, -1]) / (CIR$est[3, -1] * log(CIR$est[3, -1]))), 2) * 100,
      ",",
      round(CIR$est[3, -1]^exp(z * sqrt(CIR$var[3, -1]) / (CIR$est[3, -1] * log(CIR$est[3, -1]))), 2) * 100,
      ")"
    ))

    # graphics::mtext("No. At Risk:", side = 1, line = 3, at = at, adj = 1, cex = 0.9, font = 2)
    graphics::mtext(paste0(lg[1],', n:'), side = 1, line = line, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[1])
    graphics::mtext("CIR (%):", side = 1, line = line + 1, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[1])
    graphics::mtext("95% CI:", side = 1, line = line + 2, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[1])

    graphics::mtext(paste0(lg[2],', n:'), side = 1, line = line + 3, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[2])
    graphics::mtext("CIR (%):", side = 1, line = line + 4, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[2])
    graphics::mtext("95% CI:", side = 1, line = line + 5, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[2])

    graphics::mtext(paste0(lg[3],', n:'), side = 1, line = line + 6, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[3])
    graphics::mtext("CIR (%):", side = 1, line = line + 7, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[3])
    graphics::mtext("95% CI:", side = 1, line = line + 8, at = at, adj = 1, cex = 0.9, font = 2, col = col.list[3])


    ### Number at risk all groups
    for (i in 1:length(timepts)) {
      nindex <- which(nrisklong[, 1] == timepts[i])
      graphics::mtext(nrisklong[nindex[1], 3], side = 1, line = line, at = timepts[i], cex = .9, col = col.list[1])
      graphics::mtext(nrisklong[nindex[2], 3], side = 1, line = line + 3, at = timepts[i], cex = .9, col = col.list[2])
      graphics::mtext(nrisklong[nindex[3], 3], side = 1, line = line + 6, at = timepts[i], cex = .9, col = col.list[3])
    }

    ### Estimates for group 1
    for (i in 1:length(timepts)) {
      graphics::mtext(CIR1[i], side = 1, line = line + 1, at = timepts[i], cex = 1, col = col.list[1])
    }
    ### 95% CI for  group 1
    for (i in 1:length(timepts)) {
      graphics::mtext(confint1[i], side = 1, line = line + 2, at = timepts[i], cex = .95, col = col.list[1])
    }

    # Estimates for group 2
    for (i in 1:length(timepts)) {
      graphics::mtext(CIR2[i], side = 1, line = line + 4, at = timepts[i], cex = 1, col = col.list[2])
    }
    # 95% CI for  group 2
    for (i in 1:length(timepts)) {
      graphics::mtext(confint2[i], side = 1, line = line + 5, at = timepts[i], cex = .95, col = col.list[2])
    }

    # Estimates for group 3
    for (i in 1:length(timepts)) {
      graphics::mtext(CIR3[i], side = 1, line = line + 7, at = timepts[i], cex = 1, col = col.list[3])
    }
    # 95% CI for  group 3
    for (i in 1:length(timepts)) {
      graphics::mtext(confint3[i], side = 1, line = line + 8, at = timepts[i], cex = .95, col = col.list[3])
    }
  }

}
