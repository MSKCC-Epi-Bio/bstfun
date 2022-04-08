# if repo is hosted in an org allowed tohost PHI do the following:
# 1. Set repo to private
# 2. Add phi topic hashtag

tryCatch(
  {
    if (fs::dir_exists(fs::path(path, ".git"))) {
      # get url of origin
      remote_origin_url <-
        gert::git_remote_list(repo = path) %>%
        dplyr::filter(.data$name %in% "origin") %>%
        dplyr::pull(.data$url)

      # if repo is in a PHI org, then make repo private and add PHI tag
      if (biostatR::is_phi_repo(remote_origin_url) && rlang::is_string(remote_origin_url)) {
        repo_name <- biostatR::get_repo_name(remote_origin_url)
        org_name <- biostatR::get_org_name(remote_origin_url)

        mskRutils::use_github_msk_phi_repo(repo_name, org_name)
        mskRutils::use_github_msk_private_repo(repo_name, org_name)
      }
    }
  },
  error = function(e) {
    message(paste("Unable to set repository to 'private' and add 'phi' tag.",
                  "You must set these manually NOW if the project lives in a GH Org that allows PHI."))
  }
)

