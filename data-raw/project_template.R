## code to prepare project template goes here

# default template -------------------------------------------------------------
project_template_default <-
  list(
    readme = rlang::expr(list(
      template_filename = fs::path_package("project_templates/readme.md", package = 'bstfun'),
      filename = "README.md",
      copy = FALSE
    )),
    gitignore = rlang::expr(list(
      template_filename = fs::path_package("project_templates/gitignore.txt", package = 'bstfun'),
      filename = ".gitignore",
      copy = TRUE
    )),
    data_date = rlang::expr(list(
      template_filename = fs::path_package("project_templates/data_date.txt", package = 'bstfun'),
      filename = "data_date.txt",
      copy = FALSE
    )),
    setup = rlang::expr(list(
      template_filename = fs::path_package("project_templates/setup.Rmd", package = 'bstfun'),
      filename = glue::glue("setup.Rmd"),
      copy = FALSE
    )),
    analysis = rlang::expr(list(
      template_filename = fs::path_package("project_templates/analysis.Rmd", package = 'bstfun'),
      filename = glue::glue("analysis.Rmd"),
      copy = FALSE
    )),
    report = rlang::expr(list(
      template_filename = fs::path_package("project_templates/report.Rmd", package = 'bstfun'),
      filename = glue::glue("report.Rmd"),
      copy = FALSE
    )),
    doc_template = rlang::expr(list(
      template_filename = fs::path_package("project_templates/doc_template.docx", package = 'bstfun'),
      filename = "templates/doc_template.docx",
      copy = TRUE
    )),
    references = rlang::expr(list(
      template_filename = fs::path_package("project_templates/references.bib", package = 'bstfun'),
      filename = glue::glue("templates/references.bib"),
      copy = TRUE
    )),
    rproj = rlang::expr(list(
      template_filename = fs::path_package("project_templates/default_rproj.Rproj", package = 'bstfun'),
      filename = glue::glue("_rstudio_project.Rproj"),
      copy = TRUE
    )),
    # only add Rprofile if renv was used
    rprofile =
      rlang::expr(switch(
        renv,
        list(
          template_filename =
            fs::path_package(package = "starter", "project_templates/default_rprofile.R"),
          filename = stringr::str_glue(".Rprofile"),
          glue = TRUE
        )
      ))
  )
attr(project_template_default, "label") <- "Default Biostistics Project Template"

# hot template -----------------------------------------------------------------
hot_project_template <-
  project_template_default[c("readme", "gitignore", "data_date", "references", "doc_template", "rproj", "rprofile")] |>
  purrr::list_modify(
    setup = rlang::expr(list(
      template_filename = fs::path_package("project_templates/hot_template_files/hot_setup.Rmd", package = 'bstfun'),
      filename = glue::glue("scripts/setup1_{stringr::str_split(folder_name, pattern = ' |-', simplify = T)[, 1] %>% tolower()}.Rmd"),
      copy = FALSE
    )),
    analysis = rlang::expr(list(
      template_filename = fs::path_package("project_templates/hot_template_files/hot_analysis.Rmd", package = 'bstfun'),
      filename = glue::glue("scripts/analysis1_{stringr::str_split(folder_name, pattern = ' |-', simplify = T)[, 1] %>% tolower()}.Rmd"),
      copy = FALSE
    )),
    report = rlang::expr(list(
      template_filename = fs::path_package("project_templates/hot_template_files/hot_report.Rmd", package = 'bstfun'),
      filename = glue::glue("scripts/report1_{stringr::str_split(folder_name, pattern = ' |-', simplify = T)[, 1] %>% tolower()}.Rmd"),
      copy = FALSE
    )),
    sap = rlang::expr(list(
      template_filename = fs::path_package("project_templates/hot_template_files/hot_sap.docx", package = 'bstfun'),
      filename = glue::glue("SAP - {folder_name}.docx"),
      copy = TRUE
    )),
    doc_template = rlang::expr(list(
      template_filename = fs::path_package("project_templates/hot_template_files/doc_template.docx", package = 'bstfun'),
      filename = "scripts/templates/doc_template.docx",
      copy = TRUE
    )),
    derived_vars = rlang::expr(list(
      template_filename = fs::path_package("project_templates/derived_variables.xlsx", package = 'bstfun'),
      filename = glue::glue("scripts/derived_variables_{stringr::str_split(folder_name, pattern = ' |-', simplify = T)[, 1] %>% tolower()}.xlsx"),
      copy = TRUE
    )),
    csl = rlang::expr(list(
      template_filename = fs::path_package("project_templates/hot_template_files/european-urology.csl", package = 'bstfun'),
      filename = "scripts/templates/european-urology.csl",
      copy = TRUE
    ))
  )

attr(hot_project_template, "script_path") <-
  rlang::expr(fs::path_package("project_templates/hot_template_files/hot_script.R", package = 'bstfun'))
attr(hot_project_template, "label") <- "H.O.T. Project Template"

# whitingk template ------------------------------------------------------------
whitingk_project_template <-
  project_template_default[c("readme", "gitignore", "rproj", "rprofile")] |>
  purrr::list_modify(
    setup = rlang::expr(list(
      template_filename = fs::path_package("project_templates/whitingk_template_files/01_clean-data.R", package = 'bstfun'),
      filename = glue::glue("scripts/01_clean-data.R"),
      copy = FALSE
    )),
    analysis = rlang::expr(list(
      template_filename = fs::path_package("project_templates/whitingk_template_files/02_analysis.Rmd", package = 'bstfun'),
      filename = glue::glue("scripts/02_analysis.Rmd"),
      copy = FALSE
    ))
  )

attr(whitingk_project_template, "script_path") <-
  expression(fs::path_package("project_templates/whitingk_template_files/write_directories.R", package = 'bstfun'))
attr(whitingk_project_template, "label") <- "Karissa's Project Template"


# leej22 template ------------------------------------------------------------
leej22_project_template <-
  project_template_default[c("readme", "gitignore", "data_date", "rproj", "rprofile")] |>
  purrr::list_modify(
    setup = rlang::expr(list(
      template_filename = fs::path_package("project_templates/leej22_template_files/01_data_setup.Rmd", package = 'bstfun'),
      filename = glue::glue("code/{folder_name}_01_data_setup.Rmd"),
      copy = FALSE
    )),
    data_checks = rlang::expr(list(
      template_filename = fs::path_package("project_templates/leej22_template_files/02_data_checks.Rmd", package = 'bstfun'),
      filename = glue::glue("code/{folder_name}_02_data_checks.Rmd"),
      copy = FALSE
    )),
    analysis = list(
      template_filename = fs::path_package("project_templates/leej22_template_files/03_analysis.Rmd", package = 'bstfun'),
      filename = glue::glue("code/{folder_name}_03_analysis.Rmd"),
      copy = FALSE
    ),
    report = rlang::expr(list(
      template_filename = fs::path_package("project_templates/leej22_template_files/04_report.Rmd", package = 'bstfun'),
      filename = glue::glue("code/{folder_name}_04_report.Rmd"),
      copy = FALSE
    ))
  )
attr(leej22_project_template, "script_path") <-
  expression(fs::path_package("project_templates/leej22_template_files/write_directories.R", package = 'bstfun'))
attr(leej22_project_template, "label") <- "Jasme Lee Project Template"


# Create template object -----

project_templates <- list()
project_templates[["default"]] <- project_template_default
project_templates[["hot"]] <- hot_project_template
project_templates[["whitingk"]] <- whitingk_project_template
project_templates[["leej22"]] <- leej22_project_template

usethis::use_data(project_templates, overwrite = TRUE)
