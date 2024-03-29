% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_bst_file.R
\name{use_file}
\alias{use_file}
\alias{use_bst_file}
\alias{use_bst_gitignore}
\alias{use_bst_readme}
\alias{use_bst_setup}
\alias{use_bst_analysis}
\alias{use_bst_report}
\title{Write a template file}
\usage{
use_bst_file(name = NULL, filename = NULL, open = interactive())

use_bst_gitignore(filename = NULL)

use_bst_readme(filename = NULL)

use_bst_setup(filename = NULL)

use_bst_analysis(filename = NULL)

use_bst_report(filename = NULL)
}
\arguments{
\item{name}{Name of file to write.  Not sure of the files available to you?
Run the function without specifying a name, and all files available within the
template will print.}

\item{filename}{Optional argument to specify the name of the file to be written.
Paths/filename is relative to project base}

\item{open}{If \code{TRUE}, opens the new file.}
}
\description{
Rather than using \code{create_bst_project()} to start a new project folder, you
may use \code{use_bst_file()} to write a single file from any project template.
The functions \code{use_bst_gitignore()} and \code{use_bst_readme()} are shortcuts for
\code{use_bst_file("gitignore")} and \code{use_bst_file("readme")}.
}
\examples{
if (FALSE) {
# create gitignore file
use_bst_file("gitignore")
use_bst_gitignore()

# create README.md file
use_bst_file("readme")
use_bst_readme()
}
}
\seealso{
\code{\link[=create_bst_project]{create_bst_project()}}
}
