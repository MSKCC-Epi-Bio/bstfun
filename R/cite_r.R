#' Cite R and R Packages
#'
#' Add citations to R and R packages in an R Markdown report.
#'
#' @param pkgs character vector of package names to cite. Default is
#' `c("tidyverse", "gtsummary")`. `NULL` is acceptable.
#' @param add_citations logical indicating whether to include the bibtex citations
#' of R and the packages. Default is `TRUE`. When TRUE, we expect the R markdown
#' file to reference a bib file including the references for each package
#' listed in the `pkgs=` argument. The cite key for each of these entries
#' must match the package name. Also, the file must include an entry for
#' R with cite key 'r'.
#'
#' @section R Markdown:
#'
#' Below is an example how the `cite_r()` function would be used in an R
#' Markdown report.
#'
#' ````
#' ---
#' output: html_document
#' bibliography: references.bib
#' ---
#'
#' Analyses were conducted with `r bstfun::cite_r(pkgs = "tidyverse")`.
#' ````
#' This assumes there is a bib file of references called `references.bib` that
#' contain the following entries.
#'
#' ````
#'   @Manual{r,
#'     title = {R: A Language and Environment for Statistical Computing},
#'     author = {{R Core Team}},
#'     organization = {R Foundation for Statistical Computing},
#'     address = {Vienna, Austria},
#'     year = {2021},
#'     url = {https://www.R-project.org/},
#'   }
#'
#'   @Article{tidyverse,
#'     title = {Welcome to the {tidyverse}},
#'     author = {Hadley Wickham and Mara Averick and Jennifer Bryan and Winston Chang and Lucy D'Agostino McGowan and Romain François and #' Garrett Grolemund and Alex Hayes and Lionel Henry and Jim Hester and Max Kuhn and Thomas Lin Pedersen and Evan Miller and Stephan Milton #' Bache and Kirill Müller and Jeroen Ooms and David Robinson and Dana Paige Seidel and Vitalie Spinu and Kohske Takahashi and Davis #' Vaughan and Claus Wilke and Kara Woo and Hiroaki Yutani},
#'     year = {2019},
#'     journal = {Journal of Open Source Software},
#'     volume = {4},
#'     number = {43},
#'     pages = {1686},
#'     doi = {10.21105/joss.01686},
#'   }
#' ````
#' @export
#'
#' @examples
#' # cite R and the tidyverse
#' cite_r(pkgs = "tidyverse")
#'
#' # cite R and the tidyverse, but text only
#' cite_r(pkgs = "tidyverse", add_citations = FALSE)
#'
#' # only cite R
#' cite_r(pkgs = NULL)
cite_r <- function(pkgs = c("tidyverse", "gtsummary"),
                   add_citations = TRUE) {
  # citing R language ----------------------------------------------------------
  r_version <-
    glue::glue("v{getRversion()}") %>%
    {ifelse(add_citations, glue::glue("{.} [@r]"), .)}

  # citing R pkgs --------------------------------------------------------------
  if (!is.null(pkgs)) {
    pkg_versions <-
      map_chr(
        pkgs,
        ~ ifelse(
          add_citations,
          glue::glue("{.x} v{packageVersion(.x)} [@{.x}]"),
          glue::glue("{.x} v{packageVersion(.x)}")
        )
      )

    pkg_versions <- glue::glue_collapse(pkg_versions, sep = ", ", last = " and ")
  }

  # construct statement --------------------------------------------------------
  if (is.null(pkgs))
    statement <- glue::glue("R {r_version}")
  else if (length(pkgs) == 1L)
    statement <- glue::glue("R {r_version} with the {pkg_versions} package")
  else
    statement <- glue::glue("R {r_version} with the {pkg_versions} packages")

  # return statement -----------------------------------------------------------
  statement
}
