
#' Set bold and/or italic style for groups labels in stacked tables
#'
#' @param x a gtsummary stacked table
#' @param bold
#' @param italics
#' @param print_engine
#' @param bold TRUE|FALSE
#' @param italics TRUE|FALSE
#' @param print_engine Choose a print engine to render the table, one of
#' "gt", "flextable", "huxtable"
#' @export
#' @examplesIf interactive()


#' tbl_ex_stack <-
#'   tbl_stack(
#'
#'     list(trial %>%
#'            filter(grade == "I") %>%
#'            select(trt, age) %>%
#'            tbl_summary(by = trt),
#'
#'          trial %>%
#'            filter(grade == "II") %>%
#'            select(trt, age) %>%
#'            tbl_summary(by = trt),
#'
#'          trial %>%
#'            filter(grade == "III") %>%
#'            select(trt, age) %>%
#'            tbl_summary(by = trt)),
#'
#'     group_header = levels(trial$grade)
#'
#'   )
#'
#'tbl_ex_stack %>%
#'   modify_spanning_header(all_stat_cols() ~ "**Treatment Received**") %>%
#'   bold_italicise_group_labels(bold = TRUE,
#'                              print_engine = "flextable")
#'
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
            flextable::bold(j=stringr::str_starts(string = .$col_keys, pattern = "groupname_col"))

        },
        "huxtable" =  {
          x = gtsummary::as_hux_table(x)
          x <-
            x %>%
            huxtable::set_bold(row = stringr::str_detect(pattern = "\\.", string = rownames(.), negate = TRUE),
                               col = dplyr::starts_with("groupname_col"))
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
            flextable::italic(j=stringr::str_starts(string = .$col_keys, pattern = "groupname_col"))
        },
        "huxtable" =  {
          if (!("huxtable" %in% class(x))) {
            x <- gtsummary::as_hux_table(x)
          }
          rowsToItalic <- stringr::str_detect(pattern = "\\.", string = rownames(x), negate = TRUE)
          rowsToItalic[1] <- FALSE
          x <-
            x %>%
            # huxtables contains a dataframe with named rows,
            # each row starting with a dot is header or footer
            # with the following regex in argument "row" we
            # unselect such a rows (headers or footers)
          # huxtable::set_italic(row = stringr::str_detect(pattern = "\\.", string = rownames(.), negate = TRUE),
            huxtable::set_italic(row = rowsToItalic,
                               col = dplyr::starts_with("groupname_col"))
        }
      )
      cli::cli_alert_info("{.field gtsummary} table has been converted class {.val {print_engine}}")
    }
    x
  }
