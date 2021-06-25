## code to prepare project template goes here

# hot template
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
      filename = glue::glue("setup1.Rmd"),
      copy = FALSE
    ),
    analysis = list(
      template_filename = fs::path_package("project_templates/analysis.Rmd", package = 'bstfun'),
      filename = glue::glue("analysis1.Rmd"),
      copy = FALSE
    ),
    report = list(
      template_filename = fs::path_package("project_templates/report.Rmd", package = 'bstfun'),
      filename = glue::glue("report1.Rmd"),
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
    )
  ))
attr(project_template_default, "label") <- "Default Biostistics Project Template"

project_templates <- list()
project_templates[["default"]] <- project_template_default

usethis::use_data(project_templates, overwrite = TRUE)
