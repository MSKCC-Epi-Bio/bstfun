# write outputs for for Rmd results
fs::dir_create(path = c(fs::path(path, "raw-data"),
                        fs::path(path, "data"),
                        fs::path(path, "admin"),
                        fs::path(path, "outputs", "figures")))

usethis::ui_done("Writing subfolders")
