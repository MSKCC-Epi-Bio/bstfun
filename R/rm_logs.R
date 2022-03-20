#' Deletes log files created by Rscript on the Linux cluster
#'
#' When an R script is submitted to the server, Linux generates a log file named
#' `myRscript.R.o######`.  This program searches a folder for files named like this
#' and removes/deletes them.
#'
#' @param path folder location to search for log files
#' @param recursive logical. should log files in subdirectories also be deleted?
#' @export

rm_logs <- function(path = here::here(), recursive = FALSE) {
  # get list of files in dir
  files <- list.files(path = path)

  # subset on those with ".R.o" in the file name (or ".r.o")
  files <-
    files[grepl(pattern = ".R.o", x = files) | grepl(pattern = ".r.o", x = files)]

  # keep files that end in 6 intergers
  files <-
    files[
      files %>%
        {
          substr(., nchar(.) - 6 + 1, nchar(.))
        } %>%
        {
          !grepl("[^0-9]", .)
        }
    ]

  # deleting log files
  file.path(path, files) %>%
    file.remove()
}
