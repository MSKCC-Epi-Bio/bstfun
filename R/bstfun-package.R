#' @importFrom purrr %||% map imap map_chr map2 pmap map_lgl flatten negate
#' @importFrom dplyr select mutate mutate_at case_when filter pull vars
#'   group_by pull rename coalesce
#' @importFrom tibble tibble as_tibble
#' @importFrom forcats fct_rev as_factor
#' @importFrom stringr fixed str_replace str_detect
#' @importFrom rlang := .data .env enquo enexpr set_names
#' @importFrom glue glue
#' @keywords internal
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(c(".", "exponentiate"))

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL
