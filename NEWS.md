# bstfun (development version)

* Deprecated `reinstall_prior_pkgs()` in favor of `updater::install_pkgs()`.

* Added `ggcalibration()` function.

* Update to `add_sparkline()` to handle updated API in {gtExtras}. (#101)

* Added `use_bst_rstudio_prefs(profile=)` argument allowing users to modify the standard biostatistics preferences.

# bstfun 0.4.4

* Update to the HOT project template.

# bstfun 0.4.3

* Updated project template `.gitignore` file.

# bstfun 0.4.2

* Removed `"as_flex_table-lgl:use_ftExtra"` gtsummary theme element from HOT template. There seems to be an issue with {ftExtra} when the package is knit (but works interactively).

# bstfun 0.4.1

* Re-exporting the `here::here()` function.

* Re-exporting `gtsummary::tbl_split()` function, and no longer warning about the function's use from within {bstfun}.

# bstfun 0.4.0

* Overhaul to `create_bst_project()` including updated interface and templates.

* Updated `auc_density()` to better handle cut points and 0 and 1 and improved output structure by adding sensitivity and specificity estimates to a tibble.

* Added new functions `add_n.tbl_likert()` and `add_continuous_stat().tbl_likert()` (#55)

* Updated templates associated with `create_bst_project()` including adding a SAS template (#84)

# bstfun 0.3.7

* No longer depending on the development version of {gtsummary}.

* Improved messaging in `reinstall_prior_pkgs()`.

# bstfun 0.3.6

* Bug fix in `set_derived_variables()` moving ID variables to the front of the data frame.

* Added `add_variable_grouping()` function that can group variable summaries together under a single header. (#79)

* Adding new function `reinstall_prior_pkgs()` to assist in re-populating an R package library after a fresh install.

# bstfun 0.3.5

* Updates the the `theme_gtsummary_msk("hot")` theme.
    - Default chi-square test without continuity correction removed since it is the current default for tbl_summary. 
    - Added the compact theme to the the HOT theme.

* Added a method to calculate adjusted differences for binary endpoints using logistic regression `logistic_reg_adj_diff()`. (#20)

# bstfun 0.3.4

* No changes. Making release for RSPM.

# bstfun 0.3.3

* Bug fix in HOT project template where the `reference.bib` file was being placed in the incorrect folder.

* Columns `"mrn"`, `"id"`, or `"patient_id"` will now be moved to the front of the data frame after labels have been applied using `set_derived_variables()`.

# bstfun 0.3.2

* Fix to the H.O.T. `use_hot_file()` default template specification.

* Increased minimum R version and minimum version of {starter} package (after bug fix).

# bstfun 0.3.1

* Updates to the templating files.

* Updated custom template definitions to utilize parts from the default template, reducing the amount of code needed to create a custom template.

* Added new function `bold_italicize_group_labels()` to bold and italicize group header labels in gtsummary tables. (#65)


# bstfun 0.3.0

* Function `add_cuminc_risktables()` now allows for 3 groups. Makes colors consistent between labels and estimates.  Removed "Number at risk" label and places a label ", n:" next to each strata name. 

* Migrated the H.O.T. project templates from {hotfun} to {bstfun}.

* Added new function `clean_mrn()` to clean and check consistency of MRN columns.

* Added new function `set_derived_variables()` that imports variable labels from an excel file and applies the labels to the data frame.

* Added new function `count_map()` to aid in checking variable derivations.

* Added new function `count_na()` to assess variable missing patterns.

* Added new function `get_mode()` to obtain the mode of a variable.

* Added new function `list_labels()` to obtain a named list of column names and their labels.

* Added new function `assign_timepoint()` to aid in selecting observations at various follow-up times, e.g. selecting the 6 and 12 month lab result from a long data set of all labs.

* Added new function `add_splines()`.

* Added new functions for calculating eGFR: `egfr_mdrd()` and `egfr_ckdepi()`.

* Migrated H.O.T. project creation functions from the hotfun package.

* Added two functions for calculating AUC based on the distribution of a variable.

* Removed use of deprecated function in `tbl_likert()`.

* Added new function `followup_time()` that reports the amount of follow-up among censored patients.

* Added new function `list_labels()` to extract the variable labels from a data frame.

* Added new function `rm_logs()` to delete logs created from submitting jobs to the HPCC.

# bstfun 0.2.8

* Updated how references are added in `cite_r()`.

# bstfun 0.2.7

* Added new function `add_sparkline()`--a wrapper for `gtExtras::gt_sparkline()` that adds distributional figures in a new column of a 'tbl_summary' table.

* Updated default argument value: `as_forest_plot(xlog = x$inputs$exponentiate)`.

# bstfun 0.2.6

* Deprecated `tbl_2way_summary()` in favor of `gtsummary::tbl_continous()`.

* Cleaning up functions that have been deprecated.

* Removing old {gtsummary} "core" code that had been copied into the package.

# bstfun 0.2.5

* Deprecated `bstfun::tbl_split()`, `bstfun::gts_add_p_footnote()`, and `bstfun::gtsummary_butcher()` in lieu of `gtsummary::tbl_split()`, `gtsummary::separate_p_footnote()`, and `gtsummary::tbl_butcher()`.

* Removed {tidyselect} dependency.

* Added new function `as_forest_plot()` that converts a gtsummary table into a forest plot using `forestplot::forestplot()`.

* Added new function `add_cuminc_risktable()` to plot cumulative incidence estimates with both the at-risk table and the estimates printed below the table.

* Updated Rmd files in project template to use user's custom `gtsummary` theme if it exists within `bstfun::theme_gtsummary_msk()`, matching the system login name to the `name` in `bstfun::theme_gtsummary_msk()`

* Adding new function `use_bst_rstudio_prefs()`

* Added new functions `tbl_likert()` and `add_n.tbl_likert()` for summarizing likert-scale data. (#53) 

# bstfun 0.2.4

* Updated the references file in the {bstfun} project template.

* Exporting `get_data_date()` function used in `here_data()`

* Bug fix in `path_data()` where `getOption("path_data")` was not correctly being imported. The arguments of `path_data()` have been re-arranged.

# bstfun 0.2.3

* Update to `here_data()` to import the data date properly when the date is not followed by a hard return. (#42)

# bstfun 0.2.2

* Added function `cite_r()` to assist in citing R and R packages in R markdown reports.

* Updated default project template to include references in `report.Rmd`.

* Added function `use_varnames_as_labels()` to assign title case (or all CAP) labels from the column names of a data frame.

* Switched {starter} dependency from GitHub to CRAN.

* Bug fix in `tbl_2way_summary()`. Now forcing the continuous variable to be summarized continuously.

* Added `path_data()` function.

# bstfun 0.2.1

* Updates to project templates.

# bstfun 0.2.0

* Added new function `create_bst_project()`, a wrapper for `starter::create_project()` that drops a copy of the Biostatistics project template into a new or existing folder. The function defaults to the Biostatistics template, but any template may be passed.

* Added new functions `use_bst_file()`, `use_bst_gitignore()`, and `use_bst_readme()` to drop files from the HOT template into the active project.

* Migrated the Biostatistics template from the biostatR package (behind a firewall) to the bstfun package.

* Added function `gtsummary_butcher()` to reduce the size of a gtsummary table. After an object has been butchered, other gtsummary functions may not be able to execute on the object.

# bstfun 0.1.6

* Added `here_data(path_to_data_date=)` argument to specify location of data date file.

* Adding `add_inline_forest_plot()` function to add forest plots to gtsummary tables. (#27)

# bstfun 0.1.5

* Updated `gts_add_p_footnotes()` to maintain 'gtsummary' class, where previously the table was converted to gt or flextable.

* Added `tbl_2way_summary()` function.

* Deprecated `tbl_ancova()` in favor of `gtsummary::add_difference()`.

# bstfun 0.1.4

* Added `hpcc_get_arg()` and `hpcc_get_seq_number()` functions to aid working with the high performance computing cluster. (#19)

* Added `as_ggplot()` function to convert gt and gtsummary tables to ggplot

* Added `tbl_split()` function to actively split a gtsummary table to more easily show on multiple pages.

* Added `"leej"` theme to `theme_gtsummary_msk()`

# bstfun 0.1.3

* Added new function `style_tbl_compact()` that makes the compact styling available in `gtsummary::theme_gtsummary_compact()` to any {gt}, {flextable}, {huxtable}, or `knitr::kable()` table.

# bstfun 0.1.2

* Added new function `gts_add_p_footnotes()`

* Added new themes to `theme_gtsummary_msk()`

# bstfun 0.1.1

* Added the `theme_gtsummary_msk()` function. This is a place for any member of the MSK community to add their person gtsummary theme.

* Added the `here_data()` function. Similar to `here::here()` which returns your project directory, `here_data()` returns the path to your current data folder based on the date in `data_date.txt`.

# bstfun 0.1.0

* First release
