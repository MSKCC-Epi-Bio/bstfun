#' Set custom gtsummary themes
#'
#' This is a place for any member of the MSK community to add a personal gtsummary theme.
#' Reach out if you're interested in adding yours!
#'
#' Visit the [gtsummary themes vignette](http://www.danieldsjoberg.com/gtsummary/articles/themes.html#writing-themes-1) for a full list of preferences that can be set.
#'
#' @param name string indicating the custom theme to set.
#'
#' @export
#' @examples
#' theme_gtsummary_msk("hot")
theme_gtsummary_msk <- function(name = c("hot")) {
  # picking theme
  name <- match.arg(name)

  # selecting theme list
  switch(
    name,
    "hot" = lst_theme_hot,
    "karissa" = lst_theme_karissa
  ) %>%
    # setting theme
    gtsummary::set_gtsummary_theme()
}

# Health Outcome Teams ---------------------------------------------------------
lst_theme_hot <- list(
  "pkgwide-str:theme_name" = "H.O.T.",
  "add_p.tbl_summary-attr:test.categorical" = "chisq.test.no.correct"
)

# Karissa Whiting --------------------------------------------------------------
lst_theme_karissa <- list(
  "pkgwide-str:theme_name" = "Karissa Whiting",
  "pkgwide-fn:pvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2),
  "pkgwide-fn:prependpvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2, prepend_p = TRUE)
)
