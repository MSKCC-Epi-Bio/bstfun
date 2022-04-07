#' Start a New Biostatistics project
#'
#' Creates a directory with the essential files for a new project.
#' The function can be used on existing project directories as well.
#' This is a thin wrapper for `starter::create_project()`.
#'
#' @inheritParams starter::create_project
#' @inheritDotParams starter::create_project -template
#'
#' @name create_project
#' @examplesIf FALSE
#' if (FALSE) {
#' # specifying project folder location (folder does not yet exist)
#' project_path <- fs::path(tempdir(), "My Project Folder")
#'
#' # creating folder where secure data would be stored (typically will be a network drive)
#' secure_data_path <- fs::path(tempdir(), "secure_data")
#' dir.create(secure_data_path)
#'
#' # creating new project folder
#' create_bst_project(project_path, path_data = secure_data_path)
#' }
NULL

#' @export
#' @rdname create_project
create_bst_project <- function(path,
                               path_data = NULL,
                               ...) {
  # if template is NULL, use default template ----------------------------------
  template <- .select_template()

  # create new project ---------------------------------------------------------
  starter::create_project(
    path = path,
    path_data = path_data,
    ...
  )
}

#' @export
#' @rdname create_project
create_hot_project <- function(path, path_data = NULL, ...) {
  starter::create_project(
    path = path,
    path_data = path_data,
    template = bstfun::project_templates[["hot"]],
    ...
  )
}

.select_template <- function() {
  # if not interactive, return default template
  if (!interactive()) {
    return(bstfun::project_templates[["default"]])
  }

  # creating list of templates available
  templates <-
    list("1. Default Template" = bstfun::project_templates[["default"]],
         "2. Scripts and Results in Separate Folders" =
           bstfun::project_templates[["results_folder"]])
  # adding user-defined template if it exists
  if (!is.null(bstfun::project_templates[[tolower(Sys.info()[["user"]])]])) {
    templates <-
      c(templates,
        list(bstfun::project_templates[[tolower(Sys.info()[["user"]])]]) %>%
          rlang::set_names(stringr::str_glue("3. Personal template: {tolower(Sys.info()[['user']])}")))
  }

  # asking users which template to use
  options <- seq_len(length(templates))
  question <-
    c(stringr::str_glue("Select {glue::glue_collapse(, sep = ', ', last = ' or ')}"),
      names(templates))
  answer <- readline(question)
  if (!answer %in% options) {
    stop("Invalid template selection.", call. = FALSE)
  }

  # return selected template
  return(templates[[answer]])
}

