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
theme_gtsummary_msk <- function(name = c("hot", "karissa", "ally", "mauguen",
                                         "esther", "curry")) {
  # picking theme name
  name <- match.arg(name)

  # selecting theme list
  switch(
    name,
    "hot" = lst_theme_hot,
    "ally" = lst_theme_ally,
    "karissa" = lst_theme_karissa,
    "mauguen" = lst_theme_mauguen,
    "esther" = lst_theme_esther,
    "curry" = lst_theme_curry
  ) %>%
    # setting theme
    gtsummary::set_gtsummary_theme()
}

# Health Outcome Teams ---------------------------------------------------------
lst_theme_hot <- list(
  "pkgwide-str:theme_name" = "H.O.T.",
  "add_p.tbl_summary-attr:test.categorical" = "chisq.test.no.correct",
  "tbl_regression-str:coef_header" = rlang::expr(ifelse(exponentiate == TRUE, "exp(\U03B2)", "\U03B2"))
)

# Karissa Whiting --------------------------------------------------------------
lst_theme_karissa <- list(
  "pkgwide-str:theme_name" = "Karissa Whiting",
  "pkgwide-fn:pvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2),
  "pkgwide-fn:prependpvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2, prepend_p = TRUE),
  "tbl_regression-str:coef_header" = rlang::expr(ifelse(exponentiate == TRUE, "exp(\U03B2)", "\U03B2"))
)

# Mike Curry -------------------------------------------------------------------
lst_theme_curry <- list(
  "pkgwide-str:theme_name" = "Mike Curry",
  "pkgwide-fn:pvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2),
  "pkgwide-fn:prependpvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2, prepend_p = TRUE),
  "tbl_regression-str:coef_header" = rlang::expr(ifelse(exponentiate == TRUE, "exp(\U03B2)", "\U03B2"))
)

# Ally Maschino-Hinton ---------------------------------------------------------
lst_theme_ally <- list(
  "pkgwide-str:theme_name" = "Ally Maschino-Hinton",
  "pkgwide-fn:pvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2),
  "pkgwide-fn:prependpvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2, prepend_p = TRUE),
  "tbl_regression-str:coef_header" = rlang::expr(ifelse(exponentiate == TRUE, "exp(\U03B2)", "\U03B2"))
)

# Audrey Mauguen ---------------------------------------------------------------
lst_theme_mauguen <- list(
  "pkgwide-str:theme_name" = "Audrey Mauguen",
  "pkgwide-fn:pvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2),
  "pkgwide-fn:prependpvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2, prepend_p = TRUE),
  "tbl_summary-fn:percent_fun" = function(x) gtsummary::style_number(x, scale = 100),
  "tbl_summary-str:categorical_stat" = "{n} ({p}%)",
  "tbl_summary-str:continuous_stat" = "{median} ({min} \U2013 {max})",
  "pkgwide-str:print_engine" = "flextable"
)

# Esther Drill -----------------------------------------------------------------
lst_theme_esther <- list(
  "pkgwide-str:theme_name" = "Esther Drill",
  "pkgwide-fn:pvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2),
  "pkgwide-fn:prependpvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 2, prepend_p = TRUE)
)
