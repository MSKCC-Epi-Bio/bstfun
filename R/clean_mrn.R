#' Check and Format MRNs
#'
#' @description
#' An MRN follows specific rules
#' 1. Must be character
#' 1. Must contain only numeric components
#' 1. Must be eight characters long and include leading zeros.
#'
#' This function converts numeric MRNs to character and ensures it follows
#' MRN conventions. Character MRNs can also be passed, and leading zeros
#' will be appended and checked for consistency.
#'
#' @param x vector to be converted and checked to MRN
#' @param allow_na logical indicating whether `NA` values are accepted.
#' Default is `FALSE`
#' @param check_unique Check if MRNs are unique
#'
#' @return character MRN vector
#' @export
#'
#' @examples
#' 1000:1001 %>%
#'   clean_mrn()
clean_mrn <- function(x, allow_na = FALSE, check_unique = FALSE) {
  # convert to character -------------------------------------------------------
  if (is.numeric(x))
    x <- as.character(x)

  # adding leading zeros -------------------------------------------------------
  x <-
    stringr::str_trim(x) %>%
    stringr::str_pad(width = 8, side = "left", pad = "0")

  # checking MRN for consistency -----------------------------------------------
  if (allow_na %in% FALSE && sum(is.na(x)) > 0) {
    stop("Missing MRN values not allowed.", call. = FALSE)
  }

  # length is 8 ----------------------------------------------------------------
  if (stats::na.omit(x) %>% nchar() %>% setequal(8) %in% FALSE) {
    stop("MRN must be length 8.", call. = FALSE)
  }

  # all digits -----------------------------------------------------------------
  if (!identical(x, stringr::str_extract_all(x, pattern = "[0-9]+") %>% unlist())) {
    stop("MRN must comprise of digits only.", call. = FALSE)
  }

  # check unique ---------------------------------------------------------------
  if (check_unique && any(duplicated(x))) {
    duplicated_mrns <- x[duplicated(x)] %>% unique()
    cli::cli_alert_danger("Duplicated {.field MRNs}: {.val {duplicated_mrns}}")
    stop("MRN must be unique.", call. = FALSE)
  }

  # return formatted MRN -------------------------------------------------------
  x
}
