#' Moves p-value footnotes to individual p-values
#'
#' This function converts a gtsummary object to the output type indicated in
#' `printer=`: your object will no longer be of class gtsummary and you can
#' no longer use gtsummary functions to modify the object.
#'
#' @param x object with class `"tbl_summary"` created from the gtsummary package
#' @param printer String indicating the output format. Must be one of `"gt"`, or
#' `"flextable"`
#' @param index_start for flextable output, this is the number the footnotes begin
#' counting from
#'
#' @export
#' @examples
#' library(gtsummary)
#' trial %>%
#'   select(trt, age, grade) %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   gts_add_p_footnotes()

gts_add_p_footnotes <- function(x, printer = c("gt", "flextable"), index_start = 2) {
  printer <- match.arg(printer)

  # grouping data, one line per type of test performed
  prep_to_create_code <-
    x$meta_data %>%
    dplyr::filter(!is.na(.data$stat_test_lbl) & .data$stat_test_lbl != "") %>%
    dplyr::select(.data$variable, .data$stat_test_lbl) %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::group_nest(.data$stat_test_lbl) %>%
    dplyr::mutate(
      sort_id = purrr::map_dbl(.data$data, ~min(.x$row_id)),
    ) %>%
    dplyr::arrange(.data$sort_id) %>%
    dplyr::mutate(
      index = seq(.env$index_start, length.out = nrow(.)) %>% as.character()
    )

  # creating then executing code to add footnotes to each p-value
  if (printer == "gt") {
    # other calls to apply to x
    prepping_calls <-
      list(rlang::expr(gtsummary::modify_footnote(x, p.value ~ NA_character_)),
           rlang::expr(gtsummary::as_gt()))

    ret <-
      map2(
        prep_to_create_code$stat_test_lbl,
        prep_to_create_code$data,
        function(x, y) {
          rlang::expr(
            gt::tab_footnote(
              footnote = !!x,
              locations = gt::cells_body(
                columns = gt::vars("p.value"),
                rows = .data$variable %in% !!y$variable & .data$row_type == "label"
              )
            )
          )
        }
      ) %>%
      # concatenating expressions with %>% between each of them
      {c(prepping_calls, .)} %>%
      purrr::reduce(function(x, y) rlang::expr(!!x %>% !!y)) %>%
      # evaluating expressions
      eval()
  }
  else if (printer == "flextable") {
    assert_package("flextable", "gts_add_p_footnotes")

    # other calls to apply to x
    prepping_calls <-
      list(rlang::expr(gtsummary::modify_footnote(x, p.value ~ NA_character_)),
           rlang::expr(gtsummary::as_flex_table()))

    ret <-
      pmap(
        list(prep_to_create_code$stat_test_lbl,
             prep_to_create_code$data,
             prep_to_create_code$index),
        function(x, y, z) {
          rlang::expr(
            flextable::footnote(
              j = ~p.value,
              i = which(x$table_body$variable %in% !!y$variable &
                          x$table_body$row_type == "label"),
              part = "body",
              value = flextable::as_paragraph(!!x),
              ref_symbols = !!z
            )
          )
        }
      ) %>%
      # concatenating expressions with %>% between each of them
      {c(prepping_calls, .)} %>%
      purrr::reduce(function(x, y) rlang::expr(!!x %>% !!y)) %>%
      # evaluating expressions
      eval()
  }

  return(ret)
}


#' Check that a package is installed, stopping otherwise
#'
#' @param pkg Package required
#' @param fn Calling function from the user perspective
#'
#' @return Returns NULL or not at all.
#'
#' @noRd
#' @keywords internal
#' @author David Hugh-Jones
assert_package <- function(pkg, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    usethis::ui_oops("The {usethis::ui_value(pkg)} is required for function {usethis::ui_code(paste0(fn, '()'))}.")
    usethis::ui_todo("Install the {usethis::ui_value(pkg)} package with the code below.")
    usethis::ui_code_block('install.packages("{pkg}")')
    stop()
  }
}



