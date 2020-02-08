# THIS SCRIPT CREATES HELP FILE IMAGES FOR GT OBJECTS (INCLUDING GTSUMMARY)
library(here)
library(stringr)
library(bstfun)
rm(list = ls())

# list of all help files
gt_functions <-
  list.files(here("man")) %>%
  purrr::keep(~str_ends(., fixed(".Rd"))) %>%
  str_remove(".Rd")

# create temp bstfun firectory (example scripts will be saved here)
path_bst <- file.path(tempdir(), "bstfun")
fs::dir_create(path_bst)
unlink(path_bst) # just in case it already existed with files in folder

# cycling over each help file, and saving gt images
for (f in gt_functions) {
  usethis::ui_done(f)

  # save example code to temp file
  example_chr <- Rd2roxygen::parse_file(here("man", str_glue("{f}.Rd")))$examples
  if (is.null(example_chr)) next
  readr::write_lines(example_chr, path = file.path(path_bst, str_glue("{f}.R")))

  # run the code
  source(file.path(path_bst, str_glue("{f}.R")))

  # get list of example objects that end in "_ex###"
  example_objs <- ls()[str_ends(ls(), "_ex[:digit:]+") | str_ends(ls(), "_ex")]

  # saving an image of every gt or gtsummary example
  purrr::walk(
    example_objs,
    function(example_chr) {
      # converting string to object
      example_obj <- eval(parse(text = example_chr))
      # convert gtsummary object to gt
      if (inherits(example_obj, "gtsummary")) example_obj <- as_gt(example_obj)
      # checking object is now a gt object
      if (!(inherits(example_obj, "gt_tbl"))) return(invisible())
      # saving image
      usethis::ui_todo("Saving `{example_chr}.png`")
      gt::gtsave(example_obj,
                 filename = here("man", "figures", str_glue("{example_chr}.png")))
    }
  )

  # removing all objects except `gt_functions`, `path_bst`
  rm(list = ls()[!ls() %in% c("gt_functions", "path_bst")])
}
