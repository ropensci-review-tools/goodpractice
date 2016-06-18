
#' @include lists.R
#' @importFrom covr package_coverage zero_coverage

PREPS$covr <- function(state, path = state$path) {
  covr <- list(coverage = package_coverage(path))
  covr$zero <- zero_coverage(covr$coverage)
  state$covr <- covr
  state
}
