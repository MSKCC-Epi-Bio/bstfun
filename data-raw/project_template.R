## code to prepare project template goes here

# default template ----
project_template_default <-
  quote(list(
    readme = list(
      template_filename = fs::path_package("project_templates/readme.md", package = 'bstfun'),
      filename = "README.md",
      copy = FALSE
    ),
    gitignore = list(
      template_filename = fs::path_package("project_templates/gitignore.txt", package = 'bstfun'),
      filename = ".gitignore",
      copy = TRUE
    ),
    data_date = list(
      template_filename = fs::path_package("project_templates/data_date.txt", package = 'bstfun'),
      filename = "data_date.txt",
      copy = FALSE
    ),
    setup = list(
      template_filename = fs::path_package("project_templates/setup.Rmd", package = 'bstfun'),
      filename = glue::glue("setup.Rmd"),
      copy = FALSE
    ),
    analysis = list(
      template_filename = fs::path_package("project_templates/analysis.Rmd", package = 'bstfun'),
      filename = glue::glue("analysis.Rmd"),
      copy = FALSE
    ),
    report = list(
      template_filename = fs::path_package("project_templates/report.Rmd", package = 'bstfun'),
      filename = glue::glue("report.Rmd"),
      copy = FALSE
    ),
    doc_template = list(
      template_filename = fs::path_package("project_templates/doc_template.docx", package = 'bstfun'),
      filename = "templates/doc_template.docx",
      copy = TRUE
    ),
    rproj = list(
      template_filename = fs::path_package("project_templates/default_rproj.Rproj", package = 'bstfun'),
      filename = glue::glue("_rstudio_project.Rproj"),
      copy = TRUE
    ),
    # only add Rprofile if renv was used
    rprofile =
      switch(
        renv,
        list(
          template_filename =
            fs::path_package(package = "starter", "project_templates/default_rprofile.R"),
          filename = stringr::str_glue(".Rprofile"),
          glue = TRUE
        )
      )
  ))
attr(project_template_default, "label") <- "Default Biostistics Project Template"

# whitingk template -----

whitingk_project_template <-
  quote(list(
    readme = list(
      template_filename = fs::path_package("project_templates/readme.md", package = 'bstfun'),
      filename = "README.md",
      copy = FALSE
    ),
    gitignore = list(
      template_filename = fs::path_package("project_templates/gitignore.txt", package = 'bstfun'),
      filename = ".gitignore",
      copy = TRUE
    ),
    setup = list(
      template_filename = fs::path_package("project_templates/whitingk_template_files/01_clean-data.R", package = 'bstfun'),
      filename = glue::glue("scripts/01_clean-data.R"),
      copy = FALSE
    ),
    analysis = list(
      template_filename = fs::path_package("project_templates/whitingk_template_files/02_analysis.Rmd", package = 'bstfun'),
      filename = glue::glue("scripts/02_analysis.Rmd"),
      copy = FALSE
    ),
    report = list(
      template_filename = fs::path_package("project_templates/whitingk_template_files/03_report.Rmd", package = 'bstfun'),
      filename = glue::glue("scripts/03_report.Rmd"),
      copy = FALSE
    ),
    rproj = list(
      template_filename = fs::path_package("project_templates/default_rproj.Rproj", package = 'bstfun'),
      filename = glue::glue("_rstudio_project.Rproj"),
      copy = TRUE
    ),
    # only add Rprofile if renv was used
    rprofile =
      switch(
        renv,
        list(
          template_filename =
            fs::path_package(package = "starter", "project_templates/default_rprofile.R"),
          filename = stringr::str_glue(".Rprofile"),
          glue = TRUE
        )
      )
  ))
attr(whitingk_project_template, "label") <- "Karissa's Project Template"


# Create template object -----

project_templates <- list()
project_templates[["default"]] <- project_template_default
project_templates[["whitingk"]] <- whitingk_project_template

usethis::use_data(project_templates, overwrite = TRUE)
