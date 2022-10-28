#' Add spline terms to a data frame
#'
#' Adds spline terms calculated via `Hmisc::rcspline.eval()` to a data frame.
#'
#' @section Knot Locations:
#' Knot locations are returned in `attr(data[[variable]], "knots")`
#'
#' @param data a data frame
#' @param variable name of column in data
#' @inheritParams Hmisc::rcspline.eval
#' @param new_names Optionally specify names of new spline columns
#'
#' @return data frame
#' @export
#'
#' @examples
#' trial %>%
#'   add_splines(age)
add_splines <- function(data, variable, knots = NULL, nk = 5, norm = 2, new_names = NULL) {
  assert_package("Hmisc", "add_splines()")
  # converting variable to string if not already, and checking it's a col in data
  variable <- dplyr::select(data, {{ variable }}) %>% names()

  # calculating spline terms ---------------------------------------------------
  mtx_sp <- Hmisc::rcspline.eval(data[[variable]], knots = knots, nk = nk, norm = norm)
  df_sp <- tibble::as_tibble(mtx_sp, .name_repair = "minimal")

  # naming new spline columns --------------------------------------------------
  # check the length before applying
  if (!is.null(new_names) && length(new_names) != ncol(df_sp)) {
    stop(stringr::str_glue(
      "`new_names=` must be the same length as the number of new columns (n = {ncol(df_sp)})"
    ), call. = FALSE)
  }
  else if (is.null(new_names)) new_names <- paste0("sp", variable, seq(1, ncol(df_sp)))
  names(df_sp) <- new_names

  # check names (given or default) do not already exist in data
  if (length(intersect(names(data), new_names)) > 0) {
    stop(
      stringr::str_glue(
        "The variable(s) {glue::glue_collapse(intersect(names(data), new_names), sep = ', ')} already exist in this dataset"
      ), call. = FALSE
    )
  }

  # combining original data with splines ---------------------------------------
  df_return <-
    dplyr::bind_cols(data, df_sp) %>%
    dplyr::relocate(all_of(new_names), .after = all_of(variable))

  # attaching knot locations to the base variable column
  attr(df_return[[variable]], "knots") <- attr(mtx_sp, "knots")

  df_return
}
