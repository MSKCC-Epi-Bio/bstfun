#' Convert gt/gtsummary table to ggplot
#'
#' useful when you want to place a ggplot and gt table side-by-side.
#' To use this function you must install the magick R package AND system program
#' (see https://docs.ropensci.org/magick/articles/intro.html#installing-magick-1)
#'
#' @param x gt or gtsummary table
#' @param ... arguments passed to `gt::gtsave()`
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' library(gtsummary)
#' library(ggplot2)
#' library(patchwork)
#'
#' # convert gtsummary table to ggplot
#' tbl <-
#'   trial %>%
#'   select(age, marker, trt) %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   as_ggplot()
#'
#' # create basic ggplot
#' gg <-
#'   trial %>%
#'   ggplot(aes(x = age, y = marker, color = trt)) +
#'   geom_point()
#'
#' # stack tables using {patchwork}
#' gg / tbl
as_ggplot <- function(x, ...) {
  # checks ---------------------------------------------------------------------
  if (!inherits(x, c("gt_tbl", "gtsummary"))) stop("`x=` must be a 'gt' or 'gtsummary' table", call. = FALSE)
  assert_package("magick", "as_ggplot")
  assert_package("ggtext", "as_ggplot")

  # convert gtsummary to gt ----------------------------------------------------
  if (inherits(x, "gtsummary")) x <- gtsummary::as_gt(x)

  # save gt as image -----------------------------------------------------------
  path_gt_table_image <- fs::file_temp(ext = "png")
  gt_table_image <- gt::gtsave(x, filename = path_gt_table_image, ...)

  # save image in ggplot -------------------------------------------------------
  table_img <-
    magick::image_read(path_gt_table_image) %>%
    magick::image_ggplot(interpolate = TRUE)

  table_img
}
