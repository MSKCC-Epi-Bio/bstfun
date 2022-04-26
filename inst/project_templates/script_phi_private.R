# if repo is hosted in an org allowed to host PHI do the following:
# 1. Set repo to private
# 2. Add phi topic hashtag

tryCatch(
  {
    if (fs::dir_exists(fs::path(path, ".git"))) {
      # get url of origin
      remote_origin_url <-
        dplyr::filter(gert::git_remote_list(repo = path),
                      .data$name %in% "origin")[["url"]]

      # if repo is in a PHI org, then make repo private and add PHI tag
      if (rlang::is_string(remote_origin_url) && mskRutils::is_phi_repo(remote_origin_url)) {
        repo_name <- mskRutils::get_repo_name(remote_origin_url)
        org_name <- mskRutils::get_org_name(remote_origin_url)

        mskRutils::use_github_msk_phi_repo(repo_name, org_name)
        mskRutils::use_github_msk_private_repo(repo_name, org_name, quiet_configured = TRUE)
      }
    }
  },
  error = function(e) {
    message(paste("Unable to set repository to 'private' and add 'phi' tag.",
                  "You must set these manually NOW if the project lives in a GH Org that allows PHI."))
    message(as.character(e))
  }
)

# if project has a GH link, drop URL shortcut into data folder
if (!is.null(path_data) && fs::dir_exists(fs::path(path, ".git"))) {
  tryCatch({
    remote_origin_url <-
      gert::git_remote_list(repo = path) |>
      dplyr::filter(.data$name %in% "origin") |>
      dplyr::pull(.data$url) |>
      stringr::str_remove(".git$")

    if (!rlang::is_empty(remote_origin_url)) {
      c("[InternetShortcut]", paste0("URL=", remote_origin_url)) |>
        readr::write_lines(file = fs::path(path_data, "GitHub-Repository.url"))
      cli::cli_alert_success("Link to GitHub repository placed in data folder.")
    }
  },
  error = function(e) cli::cli_alert_danger("Failed to place link to GitHub repository in data folder")
  )
}


