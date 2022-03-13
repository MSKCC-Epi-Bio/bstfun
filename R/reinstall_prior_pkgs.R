#' Re-install pkgs from a prior installation of R
#'
#' When you install a new version of R, your package library is empty.
#' Use this function to install all the packages you had installed
#' with your previous R installation into the new R package library.
#'  - *Function installs the current release of a pkg from CRAN or a secondary repository you may have set.* Packages installed from other locations, such as GitHub, will need to be re-installed manually.
#'  - The pkgs are installed using `renv::install()` adding each package to your renv cache.
#'  - Function should be run in a fresh R Session outside of an RStudio project.
#'  - Any packages already installed will be skipped.
#'
#' @param path Path to the package library from the previous installation of R.
#' Default is `NULL`. When `NULL`, the location of the library is inferred. If
#' the location cannot be inferred, user must provide the path.
#'
#' @return `NULL`
#' @export

reinstall_prior_pkgs <- function(path = NULL) {
  # these packages are not re-installed
  base_pkgs <- c(
    "base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tools", "tcltk",
    "utils"
  )

  # path to R system library
  path <- path %||% tryCatch(.infer_previous_install_path(), error = function(e) NULL)

  if (is.null(path) || !isTRUE(dir.exists(path))) {
    paste("Could not infer path to previous R installation package library",
          "or user-specifed path does not exist.",
          "Specify the path using the `path=` argument.\n\n",
          "Please consider filing an Issue at <https://github.com/ddsjoberg/bstfun>,",
          "so we may improve the search algorithm.") %>%
      stop(call. = FALSE)
  }

  cli::cli_h1("Prior R Library {.path {path}}")

  # vct of all installed packages from former library
  old_lib_pkgs <-
    list.files(path = path) %>%
    # remove base R packages
    setdiff(base_pkgs) %>%
    # remove packages already installed
    setdiff(rownames(utils::installed.packages()))

  if (rlang::is_empty(old_lib_pkgs)) {
    cli::cli_alert_danger("No packages to install. Aborting...")
    return(invisible())
  }

  # does user want to continue
  paste("The following {length(old_lib_pkgs)} packages will be installed:",
        "{.pkg {old_lib_pkgs}}") %>%
    cli::cli_alert_info()
  install_pkgs_y_n <- readline("Do you wish to continue? [y/n] ")
  if (!identical(substr(tolower(install_pkgs_y_n), 1L, 1L), "y")) {
    cli::cli_alert_danger("Aborting...")
    return(invisible())
  }

  # install packages
  i <- 1
  cli::cli_progress_message("Installing {.pkg {old_lib_pkgs[i]}} ({i} of {length(old_lib_pkgs)})")
  for (i in seq_along(old_lib_pkgs)) {
    tryCatch({

      invisible(utils::capture.output(renv::install(old_lib_pkgs[i])))
      cli::cli_progress_update()
    },
    error = function(e) {
      cli::cli_alert_danger("{.pkg {old_lib_pkgs[i]}} could not be installed.")
    })
    cli::cli_progress_update()
  }

  return(invisible())
}

.infer_previous_install_path <- function() {
  cli::cli_alert_info("Path to previous R installation library not provided in {.code path=}.")
  cli::cli_alert_info("Searching for path now...")
  Sys.sleep(1.5)
  cli::cli_h1("Locating Prior R Installation")
  # current installation folder
  dir_current_installation <-
    R.home() %>% normalizePath() %>% fs::path_norm() %>% as.character()
  cli::cli_li("Current R Locaton {.path {dir_current_installation}}")

  # current system library
  stub_current_system_library <-
    .libPaths()[startsWith(.libPaths(), dir_current_installation)][1] %>%
    stringr::str_remove(pattern = paste0("^", dir_current_installation))
  if (rlang::is_empty(stub_current_system_library)) {
    paste("Hmmm, can't locate the R system library.",
          "Consider using the {.code path=} argument.") %>%
    cli::cli_alert_warning()
    stop("Cannot locate R system library.", call. = FALSE)
  }
  cli::cli_li("Current R System Library {.path {fs::path(dir_current_installation, stub_current_system_library)}}")

  current_r_version_folder <- basename(dir_current_installation)

  # find prior R installation folder
  prior_r_version_folder <-
    dirname(dir_current_installation) %>%
    list.dirs(recursive = FALSE) %>%
    basename() %>%
    # remove current installation
    purrr::discard(~ .x %in% current_r_version_folder) %>%
    # keep last element, assuming this is the previous release folder
    utils::tail(n = 1)

  cli::cli_li("Prior R Version {.path {prior_r_version_folder}}\n")

  # construct hypothesized path
  path <- fs::path(dirname(dir_current_installation),
                   prior_r_version_folder,
                   stub_current_system_library)

  return(path)
}
