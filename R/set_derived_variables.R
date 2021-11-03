#' Apply variable labels to data frame
#'
#' Takes labels from the Derived Variables excel file and applies them to the
#' passed data frame.
#' The excel sheet must have columns `"varname"` and `"label"`.
#'
#' @param data Data frame
#' @param path Path to Derived Variables xls/xlsx file
#' @param drop Logical indicating whether to drop unlabelled variables
#' @inheritParams readxl::read_excel
#' @author Daniel D. Sjoberg
#' @export
#' @examplesIf FALSE
#' trial %>%
#'   set_derived_variables("derived_variables_sjoberg.xlsx")

set_derived_variables <- function(data, path, sheet = NULL, drop = TRUE) {
  assert_package("readxl", "set_derived_variables()")
  # import ---------------------------------------------------------------------
  # reading in excel file of Derived Variables
  df_derived_variables <- readxl::read_excel(path = path, sheet = sheet)
  if (!c("varname", "label") %in% names(df_derived_variables) %>% any()) {
    stop("Expecting excel sheet to have columns 'varname' and 'label'.", call. = FALSE)
  }

  # variable labels ------------------------------------------------------------
  # converting imported derived variables into named list with labels
  lst_variable_labels <-
    tibble(varname = names(data)) %>%
    dplyr::inner_join(df_derived_variables, by = "varname") %>%
    dplyr::select(.data$varname, .data$label) %>%
    tidyr::spread(.data$varname, .data$label) %>%
    map(I)

  # applying the labels
  labelled::var_label(data) <- lst_variable_labels

  # drop -----------------------------------------------------------------------
  # dropping unlabelled data
  if (isTRUE(drop)) {
    data <- dplyr::select(data, dplyr::all_of(names(lst_variable_labels)))
  }

  # return ---------------------------------------------------------------------
  # returning labelled data frame
  data
}
