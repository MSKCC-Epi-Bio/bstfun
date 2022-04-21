#' Start a New Biostatistics project
#'
#' @description Creates a directory with the essential files for a new project.
#' The function can be used on existing project directories as well.
#'
#' The folder name should be structured as `"<PI Last Name> <Short Description>"`,
#' e.g. `"Sjoberg MRI detects Path Stage after Surgery"`.
#'
#' @inheritParams starter::create_project
#' @inheritDotParams starter::create_project -template -git
#'
#' @name create_project
#' @seealso [`starter::create_project()`]
#' @examplesIf FALSE
#' # specifying project folder location (folder does not yet exist)
#' project_path <- fs::path(tempdir(), "My Project Folder")
#'
#' # creating folder where secure data would be stored (typically will be a network drive)
#' secure_data_path <- fs::path(tempdir(), "secure_data")
#' dir.create(secure_data_path)
#'
#' # creating new project folder
#' create_bst_project(project_path, path_data = secure_data_path)
NULL

#' @export
#' @rdname create_project
create_bst_project <- function(path,
                               path_data = NULL,
                               git = NA,
                               ...) {
  # if template is NULL, use default template ----------------------------------
  template <- .select_template()

  # create new project ---------------------------------------------------------
  starter::create_project(
    path = path,
    path_data = path_data,
    template = template,
    git = git,
    ...
  )
}

#' @export
#' @rdname create_project
create_hot_project <- function(path, path_data = NULL, git = TRUE, ...) {
  starter::create_project(
    path = path,
    path_data = path_data,
    template = bstfun::project_templates[["hot"]],
    git = git,
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
    list("Scripts+Results in Same Folder" =
           bstfun::project_templates[["default"]],
         "Scripts+Results in Separate Folders" =
           bstfun::project_templates[["results_folder"]],
         "SAS Template" = bstfun::project_templates[["sas"]])
  # adding user-defined template if it exists
  if (!is.null(bstfun::project_templates[[tolower(Sys.info()[["user"]])]])) {
    templates <-
      c(templates,
        list(bstfun::project_templates[[tolower(Sys.info()[["user"]])]]) %>%
          rlang::set_names(stringr::str_glue("Personal template: {tolower(Sys.info()[['user']])}")))
  }

  # asking users which template to use
  answer <- utils::menu(names(templates), title = "Select a template:")

  # return selected template
  return(templates[[answer]])
}

