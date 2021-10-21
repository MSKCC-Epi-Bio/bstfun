# I guess that a function like bold_labels() may be adapted
# to modify group labels. x$table_styling structure should be
# modified as well?

# bold_group_labels <- function(x) {
#   updated_call_list <- c(x$call_list, list(bold_labels = match.call()))
#   # input checks ---------------------------------------------------------------
#   if (!inherits(x, "gtsummary")) {
#     stop("Class of 'x' must be 'gtsummary'", call. = FALSE)
#   }
#
#   # bold labels ----------------------------------------------------------------
#   x <-
#     modify_table_styling(
#       x,
#       columns = "label",
#       rows = .data$row_type == "label",
#       text_format = "bold"
#     )
#
#   x$call_list <- updated_call_list
#
#   x
# }
