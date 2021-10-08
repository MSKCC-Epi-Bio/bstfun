

tbl_likert <- function(data) {
  # making all variable factors with the same levels ---------------------------
  browser()
  levels <-
    data %>%
    map(
      function(x) {
        if (inherits(x, "factor"))
          return(attr(x, "levels"))
        else
          return(unique(x) %>% sort() %>% as.character())
      }
    ) %>%
    purrr::reduce(union) %>%
    as.list()

  data <-
    data %>%
    purrr::map_dfc(
      function(x) {
        browser()
        if (inherits(x, "factor"))
          return(forcats::fct_expand(x, !!!levels))
        else
          return(factor(x) %>% forcats::fct_expand(!!!levels))
      }
    )


}
