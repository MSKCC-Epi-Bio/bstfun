#' Function to make database fixes after import
#'
#' @param data data frame with errors
#' @param engine function to import file of database fixes
#' @param ... arguments passed to the engine function to import the database fixes
#'
#' @return updated data frame
#' @export
#'
#' @examples
#' df_fixes <-
#'   tibble::tribble(
#'     ~id, ~variable, ~value,
#'     "id == 1", "age", "56",
#'     "id == 2", "trt", "Drug C"
#'   )
#' trial %>%
#'   dplyr::mutate(id = dplyr::row_number()) %>%
#'   fix_database_error(
#'     engine = I,
#'     x = df_fixes
#'   )

fix_database_error <- function(data, engine = readr::csv, ...) {
  # importing list of database fixes -------------------------------------------
  database_fixes <- do.call(engine, list(...))

  # checking inputs ------------------------------------------------------------
  # checking the names of the imported database fixes
  if (!identical(names(database_fixes), c("id", "variable", "value"))) {
    stop("Expecting a database fix file with columns `id`, `variable`, and `value`.",
         call. = FALSE)
  }

  # checking all variables are in `data=`
  if (any(!unique(database_fixes$variable) %in% names(data))) {
    missing_vars <-
      unique(database_fixes$variable)[!unique(database_fixes$variable) %in% names(data)] %>%
      {glue::glue_collapse(sQuote(.), sep = ", ", last = ", and ")}
    stop(glue("Columns {missing_vars} are not in `data`."), call. = FALSE)
  }

  # checking there are no duplicates list in file
  dup_n <- database_fixes %>% group_by(.data$id, .data$variable) %>%
    mutate(n = dplyr::n()) %>% pull(.data$n) %>% max()
  if (dup_n > 1) {
    stop("There are duplicates in the database fixes.", call. = FALSE)
  }

  # performing database fixes in data ------------------------------------------
  database_fixes2 <-
    database_fixes %>%
    mutate(
      id_expr = map(.data$id, rlang::parse_expr),
      nrows_modified = purrr::map_int(
        .data$id_expr, ~with(data, eval(.x)) %>% sum()),
      variable_type = map_chr(.data$variable, ~ class(data[[.x]])[1]),
      value_type = pmap(
        list(.data$variable, .data$variable_type, .data$value),
        function(variable, variable_type, value) {
          switch(
            variable_type,
            "character" = as.character(value),
            "logical" = as.logical(value),
            "integer" = as.integer(value),
            "numeric" = as.numeric(value)
            # factor is not working
            # "factor" =
            #   ifelse(
            #     as.character(value) %in% attr(data[[variable]], "levels"),
            #     # type is factor and new value is a level in the factor
            #     factor(value, levels = attr(data[[variable]], "levels")),
            #     # type is factor and new value is NOT a level in the factor
            #     forcats::fct_unify(list(data[[variable]], factor(value)))
            #   )
            ## ADD DATE, TIME, and TIMEDIFF TYPES...ANYTHING ELSE!?!
          ) %||% stop("ERROR")

        }
      )
    )
  database_fixes2

  # more checking --------------------------------------------------------------
  # checking there is a value type of each correction
  values_error <- purrr::map_lgl(database_fixes2$value_type, is.na) %>% sum()

  if(sum(values_error) > 0) {
    print(database_fixes2[values_error, c("variable", "value")])
    stop(paste("Could not identify variable type/class.",
               "If the class is a Base R class, please file a GitHub issue."),
         call. = FALSE)
  }

  # checking expression in id results in a logical vector
  if (!purrr::every(database_fixes2$id_expr, ~is.logical(with(data, eval(.x))))) {
    stop("Each expression in `id` column must evaluate to a logical.", call. = FALSE)
  }

  # make changes in database ---------------------------------------------------
  data_updated <- data
  for (i in 1:nrow(database_fixes2)) {
    data_updated[with(data, eval(database_fixes2$id_expr[[i]])), database_fixes2$variable[i]] <-
      database_fixes2$value_type[[i]]
  }

  # checking updated data against old data -------------------------------------
  if (!identical(names(data), names(data_updated))) {
    stop("Error in column names after updating data.", call. = FALSE)
  }

  if (!identical(map_chr(data, ~class(.x)[1]), map_chr(data_updated, ~class(.x)[1]))) {
    stop("Error where column class changed after updating data.", call. = FALSE)
  }

  # returning updated data ------------------------------------------------------
  data_updated
}
