#' Calculates the mode(s) of a set of values
#'
#' This function calculates the most common value(s) of a given set
#'
#' @param x A variable or vector (numeric, character or factor)
#' @param moden If there are multiple modes, which mode to use. The default is the first mode.
#' @param quiet By default, messages are printed if multiple modes are selected. To hide these messages, set `quiet` to `TRUE`
#'
#' @return A vector of length 1 containing the mode
#' @export
#'
#' @examples
#' get_mode(trial$stage)
#' get_mode(trial$trt)
#' get_mode(trial$response)
#' get_mode(trial$grade)
get_mode <- function(x, moden = 1, quiet = FALSE) {

  # Vector of all unique values, excluding NA values
  ux <- unique(x[!is.na(x)])

  # Count number of observations with each unique value
  tab <- tabulate(match(x, ux))

  # Return vector of most common value(s)
  modes <- sort(ux[tab == max(tab)])

  # If there is more than one mode, choose the first and print a message
  if (length(modes) == 1) {
    final_mode <- modes[1]
  } else if (length(modes) > 1) {
    final_mode <- modes[moden]

    # If moden wasn't specified, show message
    if (length(intersect(names(as.list(match.call())), "moden")) == 0 &
        quiet == FALSE) {
      message(
        glue::glue(
          "There are multiple modes in this dataset: ",
          glue::glue_collapse(modes, sep = " "),
          ". By default, the first mode '{final_mode}' was selected. ",
          "The selected mode can be changed using the `moden=` option."
        )
      )
    }
  }

  return(final_mode)
}
