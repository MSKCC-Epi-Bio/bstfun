#' Use Variable Names as Labels
#'
#' @param data a data frame
#' @param caps variables to be entirely capitalized
#' @param exclude variables to exclude from labeling
#'
#' @return a labeled data frame
#' @export
#'
#' @examples
#' library(gtsummary)
#'
#' mtcars %>%
#'   select(mpg, cyl, vs, am, carb) %>%
#'   use_varnames_as_labels(caps = c(mpg, vs, am), exclude = cyl) %>%
#'   tbl_summary() %>%
#'   as_kable(format = "simple")
use_varnames_as_labels <- function(data, caps = NULL, exclude = NULL) {
  # check inputs ---------------------------------------------------------------
  assert_package("labelled")
  stopifnot(is.data.frame(data))

  # evaluate caps and exclude arguments ----------------------------------------
  exclude <- data %>% select({{ exclude }}) %>% names()
  caps <- data %>% select({{ caps }}) %>% names() %>% setdiff(exclude)

  # variables that will be set to title case -----------------------------------
  title <- setdiff(names(data), c(caps, exclude))

  # initialize empty list that labels will be added to (as a named list)
  label_list <- list()

  # title case labels ----------------------------------------------------------
  if (!rlang::is_empty(title)) {
    label_list <- stringr::str_replace_all(title, "_", " ") %>%
      stringr::str_to_lower(.) %>%
      stringr::str_to_title(.) %>%
      as.list() %>%
      stats::setNames(title) %>%
      c(label_list)
  }

  # ALL CAPS labels ------------------------------------------------------------
  if (!rlang::is_empty(caps)) {
    label_list <- stringr::str_to_upper(caps) %>%
      as.list() %>%
      stats::setNames(caps) %>%
      c(label_list)
  }

  # label the data -------------------------------------------------------------
  data %>%
    labelled::set_variable_labels(
      .labels = label_list,
      .strict = FALSE
    )
}
