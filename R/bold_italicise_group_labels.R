
bold_group_labels <- function(x, print_engine = c("gt", "flextable", "huxtable")) {
  # input checks ---------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Class of 'x' must be 'gtsummary'", call. = FALSE)
  }
  print_engine <- match.arg(print_engine)

  # convert tbl to output type -------------------------------------------------
  result <-
    switch(
      print_engine,
      "gt" = gtsummary::as_gt(x),
      "flextable" = gtsummary::as_flex_table(x),
      "huxtable" = gtsummary::as_hux_table(x),
    )
  cli::cli_alert_info("{.field gtsummary} table has been converted class {.val {print_engine}}")

  # apply bold code to table ---------------------------------------------------
  # THE GROUP COLUMNS WILL ALWAYS BEGIN WITH 'groupname_col*'
  # APPLY THE FORMATTING TO THOSE COLUMNS
  switch(
    print_engine,
    # "gt" = gtsummary::as_gt(x),
    "gt" = {
      result <-
        result %>%
        gt::tab_options(row_group.font.weight = "bold")
           },
    # "flextable" = {},
    # "huxtable" =  {},
  )
  result
}



italicise_group_labels <- function(x, print_engine = c("gt", "flextable", "huxtable")) {
  # input checks ---------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Class of 'x' must be 'gtsummary'", call. = FALSE)
  }
  print_engine <- match.arg(print_engine)

  # convert tbl to output type -------------------------------------------------
  result <-
    switch(
      print_engine,
      "gt" = gtsummary::as_gt(x),
      "flextable" = gtsummary::as_flex_table(x),
      "huxtable" = gtsummary::as_hux_table(x),
    )
  cli::cli_alert_info("{.field gtsummary} table has been converted class {.val {print_engine}}")

  # apply bold code to table ---------------------------------------------------
  # THE GROUP COLUMNS WILL ALWAYS BEGIN WITH 'groupname_col*'
  # APPLY THE FORMATTING TO THOSE COLUMNS
  switch(
    print_engine,
    # "gt" = gtsummary::as_gt(x),
    "gt" = {
      result <-
        result %>%
        gt::tab_style(style = cell_text(style = "italic"), locations = cells_row_groups())
    },
    # "flextable" = {},
    # "huxtable" =  {},
  )
  result
}
