# write outputs for for Rmd results
fs::dir_create(path = fs::path(path, "outputs"))
usethis::ui_done("Writing folder {usethis::ui_value(fs::path(path, 'outputs'))}")
