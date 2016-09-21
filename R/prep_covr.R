
#' @include lists.R
#' @importFrom covr package_coverage zero_coverage
#' @importFrom withr with_options

PREPS$covr <- function(state, path = state$path) {
  covr <- list(coverage = package_coverage(path))
  with_options(
    list(covr.rstudio_source_markers = FALSE),
    covr$zero <- zero_coverage(covr$coverage)
  )
  state$covr <- covr
  state
}
