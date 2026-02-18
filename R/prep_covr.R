
#' @include lists.R prep_utils.R
#' @importFrom covr package_coverage zero_coverage percent_coverage
#' @importFrom withr with_options

PREPS$covr <- function(state, path = state$path, quiet) {
  state <- run_prep_step(state, "covr", function() {
    list(coverage = package_coverage(path, quiet = quiet))
  }, quiet = quiet)

  if (!inherits(state$covr, "try-error")) {
    with_options(
      list(covr.rstudio_source_markers = FALSE),
      state$covr$zero <- zero_coverage(state$covr$coverage)
    )
    state$covr$pct_by_line <- percent_coverage(state$covr$coverage, by = "line")
    state$covr$pct_by_expr <- percent_coverage(state$covr$coverage, by = "expression")
  }

  state
}
