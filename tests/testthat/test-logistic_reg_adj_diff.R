# THIS CAN BE UNCOMMENTED OUT AFTER THE NEXT GTSUMMARY RELEASE (bug fix)
# test_that("logistic_reg_adj_diff() works", {
#   tbl <-
#     gtsummary::tbl_summary(
#       trial, by = trt, include  = response, missing = "no")
#
#   expect_error(
#     tbl %>%
#       gtsummary::add_difference(
#         test = everything() ~ logistic_reg_adj_diff,
#         adj.vars = "stage"
#       ),
#     NA
#   )
#
#   expect_error(
#     tbl %>%
#       gtsummary::add_difference(
#         test = everything() ~
#           purrr::partial(logistic_reg_adj_diff,
#                          ci_type = "centile", boot_n = 100),
#         adj.vars = "stage"
#       ),
#     NA
#   )
# })
