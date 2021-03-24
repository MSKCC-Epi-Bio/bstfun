#' HPCC Functions
#'
#' @description
#' Functions for working with the high performance computing cluster
#'
#' - `hpcc_get_arg()` retrieve character argument passed from terminal,
#' e.g. `qsubR bootstrap_analysis.R "mets"`
#' - `hpcc_get_seq_number()` retrieve sequence integer when a sequence of jobs
#' were submitted from the terminal, e.g `qsubR -a 1-50%10 bootstrap_analysis.R`
#' @name hpcc
NULL

#' @rdname hpcc
#' @export
hpcc_get_arg <- function() {
  commandArgs(trailingOnly=TRUE)
}

#' @rdname hpcc
#' @export
hpcc_get_seq_number <- function() {
  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
}
