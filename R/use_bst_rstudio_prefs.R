#' Use Biostatistics RStudio Preferences
#'
#' @description
#' The function wraps `rstudio.prefs::use_rstudio_prefs()` and sets the following
#' preferences in RStudio.
#'
#' ```{r, echo = FALSE}
#' bst_prefs %>%
#'   purrr::map(as.character) %>%
#'   unlist() %>%
#'   tibble::enframe() %>%
#'   rlang::set_names(c("**Preference**", "**Value**")) %>%
#'   knitr::kable()
#' ```
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' use_bst_rstudio_prefs()
#' }}
use_bst_rstudio_prefs <- function() {
  # apply preferences ----------------------------------------------------------
  rstudio.prefs::use_rstudio_prefs(!!!bst_prefs)
}

# save preferences in list -----------------------------------------------------
bst_prefs <-
  list(always_save_history = FALSE,
       load_workspace = FALSE,
       margin_column = 80L,
       rainbow_parentheses = TRUE,
       restore_last_project = FALSE,
       rmd_chunk_output_inline = FALSE,
       show_hidden_files = TRUE,
       show_invisibles = TRUE,
       show_last_dot_value = TRUE,
       show_line_numbers = TRUE,
       show_margin = TRUE,
       save_workspace = "never")
