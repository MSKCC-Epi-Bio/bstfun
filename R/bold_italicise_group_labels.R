# I guess that a function like bold_labels() may be adapted
# to modify group labels. x$table_styling structure should be
# modified as well?

bold_group_labels <- function(x, print_engine = c("gt", "flextable", "huxtable")) {
  # input checks ---------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Class of 'x' must be 'gtsummary'", call. = FALSE)
  }
  print_engine <- match.arg(print_engine)

  # convert tbl to output type -------------------------------------------------
  result <-
    swtich(
      print_engine,
      "gt" = gtsummary::as_gt(x),
      "flextable" = gtsummary::as_flex_table(x),
      "huxtable" = gtsummary::as_hux_table(x),
    )

  # apply bold code to table ---------------------------------------------------
  # THE GROUP COLUMNS WILL ALWAYS BEGIN WITH 'groupname_col*'
  # APPLY THE FORMATTING TO THOSE COLUMNS

  result
}
