# if repo is hosted at https://github.mskcc.org/Analytic-Projects do the following:
# 1. Set repo to private
# 2. Add phi topic hashtag

tryCatch(
  {
    # get url of origin
    remote_origin_url <-
      gert::git_remote_list(repo = path) |>
      dplyr::filter(.data$name %in% "origin") |>
      dplyr::pull(.data$url)


    # if repo is in Analytic-Projects org, then make repo private and add PHI tag
    if (isTRUE(
      stringr::str_detect(stringr::fixed(remote_origin_url),
                          pattern = stringr::fixed("github.mskcc.org/Analytic-Projects")))) {
      # save repo name
      repo_name <-
        remote_origin_url |>
        fs::path_file() |>
        fs::path_ext_remove()

      hotverse::use_github_msk_private_repo(repo = repo_name, org = "Analytic-Projects")
      hotverse::use_github_msk_phi_repo(repo = repo_name, org = "Analytic-Projects")
    }
  },
  error = function(e) {
    message(paste("Unable to set repository to 'private' and add 'phi' tag.",
                  "You must set these manually NOW."))
  }
)

