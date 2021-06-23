#' Title
#'
#' @param data
#' @param caps
#' @param exclude
#'
#' @return
#' @export
#'
#' @examples
use_varnames_as_labels <- function(data, caps = NULL, exclude = NULL) {

  exclude = data %>% select({{ exclude }}) %>% names()

  caps = data %>% select({{ caps }}) %>% names() %>% setdiff(exclude)

  title_names = setdiff(names(data), c(caps, exclude))

  title_labels = list()

  if (!rlang::is_empty(title_names)) {
    title_labels <- stringr::str_replace_all(title_names, "_", " ") %>%
      stringr::str_to_lower(.) %>%
      stringr::str_to_title(.) %>%
      as.list() %>%
      setNames(title_names) %>%
      c(title_labels)
  }

  if (!rlang::is_empty(caps)) {
    title_labels <- stringr::str_to_upper(caps) %>%
      as.list() %>%
      setNames(caps) %>%
      c(title_labels)
  }

  data_labelled <- data %>% labelled::set_variable_labels(.labels = title_labels,
                                                      .strict = FALSE)

  return(data_labelled)
}
