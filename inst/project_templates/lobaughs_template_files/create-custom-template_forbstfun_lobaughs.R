
lobaughs_project_template <-
  project_template_default[c("gitignore", "data_date", "rproj", "rprofile")] |>
  purrr::list_modify(
    readme = rlang::expr(list(
      template_filename = "C:/Users/lobaughs/GitHub/_repository-shell-folder/repository-shell/SAS/lobaughs_template_files/readme.md",
      filename = "README.md",
      copy = FALSE
    )),
    setupSAS = rlang::expr(list(
      template_filename = "C:/Users/lobaughs/GitHub/_repository-shell-folder/repository-shell/SAS/lobaughs_template_files/010-data-setup.sas",
      filename = glue::glue("010-data-setup.sas"),
      copy = FALSE
    )),
    formatsSAS = rlang::expr(list(
      template_filename = "C:/Users/lobaughs/GitHub/_repository-shell-folder/repository-shell/SAS/lobaughs_template_files/_formats.sas",
      filename = glue::glue("_formats.sas"),
      copy = FALSE
    )),
    analysisSAS = rlang::expr(list(
      template_filename = "C:/Users/lobaughs/GitHub/_repository-shell-folder/repository-shell/SAS/lobaughs_template_files/020-analysis.sas",
      filename = glue::glue("020-analysis.sas"),
      copy = FALSE
    )),
    reportSAS = rlang::expr(list(
      template_filename = "C:/Users/lobaughs/GitHub/_repository-shell-folder/repository-shell/SAS/lobaughs_template_files/030-report1.sas",
      filename = glue::glue("030-report1.sas"),
      copy = FALSE
    )),
    prepSAS = rlang::expr(list(
      template_filename = "C:/Users/lobaughs/GitHub/_repository-shell-folder/repository-shell/SAS/lobaughs_template_files/_prep.sas",
      filename = glue::glue("_prep.sas"),
      copy = FALSE
    )),
    setup = rlang::expr(list(
      template_filename = "C:/Users/lobaughs/GitHub/_repository-shell-folder/repository-shell/SAS/lobaughs_template_files/010-data-setup.Rmd",
      filename = glue::glue("010-data-setup.Rmd"),
      copy = FALSE
    )),
    analysis = rlang::expr(list(
      template_filename = "C:/Users/lobaughs/GitHub/_repository-shell-folder/repository-shell/SAS/lobaughs_template_files/020-analysis.Rmd",
      filename = glue::glue("020-analysis.Rmd"),
      copy = FALSE
    )),
    report = rlang::expr(list(
      template_filename = "C:/Users/lobaughs/GitHub/_repository-shell-folder/repository-shell/SAS/lobaughs_template_files/030-report1.Rmd",
      filename = glue::glue("030-report1.Rmd"),
      copy = FALSE
    )),
  )
attr(lobaughs_project_template, "label") <- "Stephanie Lobaugh Project Template"

