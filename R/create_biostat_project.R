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
  if (is.null(rlang::dots_list(...)[["template"]])) {
    cli::cli_alert_info("To pass a custom template, use {.code starter::create_project(template=)}")
  }

  # if path is a git project, set `git = TRUE`
  if (.is_git(path) && is.na(git)) {
    git <- TRUE
  }

  # check that `path_data=` looks like a data folder ---------------------------
  if (interactive() &&
      !is.null(path_data) &&
      !tolower(basename(path_data)) %in% c("secure_data", "data")) {
    path_data_continue <-
      utils::menu(
        c("Yes", "No"),
        title =
          paste("Expecting `path_data=` path to end in a folder called 'secure_data' or 'data'.",
                "Do you wish to continue?", sep = "\n")
      )
    if (path_data_continue == 2L) {
      cli::cli_alert_danger("Aborting...")
      return(invisible())
    }
  }

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


.is_git <- function(path) {
  isTRUE(fs::dir_exists(fs::path(path, ".git")))
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
           bstfun::project_templates[["results_folder"]])
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

