

bold_italicise_group_labels <-
  function(x,
           bold = FALSE,
           italics = FALSE,
           print_engine = c("gt", "flextable", "huxtable")) {

    # input checks ---------------------------------------------------------------
    if (!inherits(x, "gtsummary")) {
      stop("Class of 'x' must be 'gtsummary'", call. = FALSE)
    }
    print_engine <- match.arg(print_engine)

    # apply bold code to table ---------------------------------------------------
    # THE GROUP COLUMNS WILL ALWAYS BEGIN WITH 'groupname_col*'
    # APPLY THE FORMATTING TO THOSE COLUMNS
    if (bold == TRUE) {
      switch(
        print_engine,
        "gt" = {
          x <- gtsummary::as_gt(x)
          x <-
            x %>%
            gt::tab_options(row_group.font.weight = "bold")
        },
        "flextable" = {
          x <- gtsummary::as_flex_table(x)
          x <-
            x %>%
            flextable::bold(j="groupname_col")
        },
        "huxtable" =  {
          x = gtsummary::as_hux_table(x)
          x <-
            x %>%
            huxtable::set_bold(row = 3:nrow(.)-1, col = 1)
        }
      )
      cli::cli_alert_info("{.field gtsummary} table has been converted class {.val {print_engine}}")
    }

    # apply italics code to table ---------------------------------------------------

    if (italics == TRUE) {
      switch(
        print_engine,
        "gt" = {
          if (!("gt_tbl" %in% class(x))) {
            x <- gtsummary::as_gt(x)
          }
          x <-
            x %>%
            gt::tab_style(style = cell_text(style = "italic"),
                          locations = cells_row_groups())
        },
        "flextable" = {
          if (!("flextable" %in% class(x))) {
            x <- gtsummary::as_flex_table(x)
          }

          x <- x %>%
            flextable::italic(j = "groupname_col")
        },
        "huxtable" =  {
          if (!("huxtable" %in% class(x))) {
            x <- gtsummary::as_hux_table(x)
          }
          x <-
            x %>%
            huxtable::set_italic(row = 3:nrow(.)-1, col = 1)
        }
      )
      cli::cli_alert_info("{.field gtsummary} table has been converted class {.val {print_engine}}")
    }
    x
  }
