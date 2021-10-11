# write additional directories
fs::dir_create(path = fs::path(path, "reports"))
usethis::ui_done("Writing folder {usethis::ui_value(fs::path(path, 'reports'))}")


fs::dir_create(path = fs::path(path, "reports/sent_to_pi"))
usethis::ui_done("Writing folder {usethis::ui_value(fs::path(path, 'reports/sent_to_pi'))}")


fs::dir_create(path = fs::path(path, "docs"))
usethis::ui_done("Writing folder {usethis::ui_value(fs::path(path, 'docs'))}")


fs::dir_create(path = fs::path(path, "manuscript"))
usethis::ui_done("Writing folder {usethis::ui_value(fs::path(path, 'manuscript'))}")
