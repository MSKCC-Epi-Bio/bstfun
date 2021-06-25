#' Write a template file
#'
#' Rather than using `create_biostat_project()` to start a new project folder, you
#' may use `use_biostat_file()` to write a single file from any project template.
#' The functions `use_biostat_gitignore()` and `use_biostat_readme()` are shortcuts for
#' `use_biostat_file("gitignore")` and `use_biostat_file("readme")`.
#'
#' @inheritParams starter::use_project_file
#' @inheritParams create_biostat_project
#' @name use_biostat_file
#' @rdname use_biostat_file
#' @seealso [`create_biostat_project()`]
#' @export
#' @examples
#' \donttest{\dontrun{
#' # create gitignore file
#' use_project_file("gitignore")
#' use_project_gitignore()
#'
#' # create README.md file
#' use_project_file("readme")
#' use_project_readme()
#' }}

use_biostat_file <- function(name = NULL,
                             filename = NULL,
                             template = NULL,
                             open = interactive()) {
  starter::use_project_file(name = name, filename = filename,
                            template = template, open = open)
}

#' @rdname use_biostat_file
#' @export
use_biostat_gitignore <- function(filename = NULL, template = NULL) {
  use_biostat_file(name = "gitignore", filename = filename,
                   template = template)
}

#' @rdname use_biostat_file
#' @export
use_biostat_readme <- function(filename = NULL, template = NULL) {
  use_biostat_file(name = "readme", filename = filename,
                   template = template)
}

