# bstfun (development version)

* Added `tbl_2way_summary()` function.

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
