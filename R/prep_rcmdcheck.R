
#' @include lists.R prep_utils.R
#' @importFrom rcmdcheck rcmdcheck

PREPS$rcmdcheck <- function(state, path = state$path, quiet) {
  if (is.null(state)) {
    state <- paste(
      "Run 'R CMD check' via the 'rcmdcheck' package. ~200 checks for",
      "documentation, namespace, compilation, tests, vignettes,",
      "CRAN compliance."
    )
  } else {
    path <- normalizePath(path)
    state <- run_prep_step(state, "rcmdcheck", function(path, quiet) {
      rcmdcheck(path, quiet = quiet)
    }, path = path, quiet = quiet, silent = quiet)
  }
  state
}
