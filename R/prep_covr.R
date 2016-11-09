
#' @include lists.R
#' @importFrom covr package_coverage zero_coverage percent_coverage
#' @importFrom withr with_options

PREPS$covr <- function(state, path = state$path, quiet) {
  covr <- list(coverage = package_coverage(path, quiet = quiet))
  with_options(
    list(covr.rstudio_source_markers = FALSE),
    covr$zero <- zero_coverage(covr$coverage)
  )
  covr$pct_by_line <- percent_coverage(covr$coverage, by = "line")
  covr$pct_by_expr <- percent_coverage(covr$coverage, by = "expression")
  state$covr <- covr
  state
}
