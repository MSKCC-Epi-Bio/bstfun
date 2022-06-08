#' Health Outcomes Team Member
#'
#' Returns logical whether user is a member of the Health Outcomes Team
#'
#' @return logical
#' @noRd
#'
#' @examples
#' print(Sys.info()[["user"]])
#' is_hot()

is_hot <- function() {
  tolower(Sys.info()[["user"]]) %in%
    # stats team
    c("sjobergd", "vickersa", "vertosie", "tinl", "asselm", "benfantn", "hollanj3", "porwals",
      # research fellows
      "scuderis", "beechb",
      # amplio team
      "aikenw", "chenl3", "woods")
}

# NOT EXPORTING AT THIS TIME!
