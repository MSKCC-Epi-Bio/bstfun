# write results dir for for Rmd output
fs::dir_create(path = fs::path(path, "results"))
usethis::ui_done("Writing subfolders")

source(
  fs::path_package("project_templates/script_phi_private.R", package = 'bstfun'),
  local = TRUE
)
