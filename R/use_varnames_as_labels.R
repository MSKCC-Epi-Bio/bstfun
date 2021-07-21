#' Use Variable Names as Labels
#'
#' @param data a dataframe
#' @param caps variables to be entirely capitalized
#' @param exclude variables to exclude from labeling
#'
#' @return a labeled dataframe
#' @export
#'
#' @examples
#' library(gtsummary)
#'
#' mtcars %>%
#'   use_varnames_as_labels(caps = vs, exclude = mpg) %>%
#'   tbl_summary() %>%
#'   as_kable()
use_varnames_as_labels <- function(data, caps = NULL, exclude = NULL) {
  exclude <- data %>%
    select({{ exclude }}) %>%
    names()

  caps <- data %>%
    select({{ caps }}) %>%
    names() %>%
    setdiff(exclude)

  title <- setdiff(names(data), c(caps, exclude))

  label_list <- list()

  if (!rlang::is_empty(title_names)) {
    label_list <- stringr::str_replace_all(title, "_", " ") %>%
      stringr::str_to_lower(.) %>%
      stringr::str_to_title(.) %>%
      as.list() %>%
      stats::setNames(title) %>%
      c(label_list)
  }

  if (!rlang::is_empty(caps)) {
    label_list <- stringr::str_to_upper(caps) %>%
      as.list() %>%
      stats::setNames(caps) %>%
      c(label_list)
  }

  data_labelled <- data %>%
    labelled::set_variable_labels(
      .labels = label_list,
      .strict = FALSE
    )

  return(data_labelled)
}
