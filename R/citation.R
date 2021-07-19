
cite_r <- function(pkgs = c("tidyverse", "gtsummary"),
                   cite_r = TRUE,
                   cite_pkgs = TRUE,
                   text = "Analyses were conducted using R {r} utilizing packages {pkgs}.") {
  # citing R language ----------------------------------------------------------
    r <-
      ifelse(
        isTRUE(cite_r),
        glue::glue("v{getRversion()} [@r]"),
        glue::glue("v{getRversion()}")
      )

    # citing R pkgs ------------------------------------------------------------

}
