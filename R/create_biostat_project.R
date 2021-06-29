#' Start a New Biostatistics project
#'
#' Creates a directory with the essential files for a new project.
#' The function can be used on existing project directories as well.
#' This is a thin wrapper for `starter::create_project()` that
#' sets the default template to `template = hotfun::project_template`
#'
#' @param template Specifies template for `starter::create_project(template=)`.
#' Default is the template in `bstfun::project_templates` whose name matches
#' the lowercase system username, if it exists;
#' otherwise, `bstfun::project_templates[["default"]]`
#' @inheritParams starter::create_project
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' # specifying project folder location (folder does not yet exist)
#' project_path <- fs::path(tempdir(), "My Project Folder")
#'
#' # creating folder where secure data would be stored (typically will be a network drive)
#' secure_data_path <- fs::path(tempdir(), "secure_data")
#' dir.create(secure_data_path)
#'
#' # creating new project folder
#' create_bst_project(project_path, path_data = secure_data_path)
#' }}
create_bst_project <- function(path,
                                   path_data = NULL,
                                   template = NULL,
                                   git = NA,
                                   renv = TRUE,
                                   overwrite = NA,
                                   open = interactive()) {
  # if template is NULL, use default template ----------------------------------
  template <-
    template %||%
    bstfun::project_templates[[tolower(Sys.info()[["user"]])]] %||%
    bstfun::project_templates[["default"]]

  # if string, select template among bstfun saved templates --------------------
  if (rlang::is_string(template)) {
    if (!template %in% names(bstfun::project_templates)) {
      paste("Pass one of the following template names or a user-defined template:",
            paste(names(bstfun::project_templates), collapse = ", ")) %>%
      stop(call. = FALSE)
    }
    template <- bstfun::project_templates[[template]]
  }

  # create new project ---------------------------------------------------------
  starter::create_project(
    path = path,
    path_data = path_data,
    git = git,
    renv = renv,
    template = template,
    overwrite = overwrite,
    open = open
  )
}
