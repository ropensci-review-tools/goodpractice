
#' @include lists.R prep_utils.R
#' @importFrom rcmdcheck rcmdcheck

PREPS$rcmdcheck <- function(state, path = state$path, quiet) {
  if (is.null(state)) {
    state <- "Run 'R CMD check' via the 'rcmdcheck' package"
  } else {
    path <- normalizePath(path)
    state <- run_prep_step(state, "rcmdcheck", function(path, quiet) {
      rcmdcheck(path, quiet = quiet)
    }, path = path, quiet = quiet, silent = quiet)
  }
  state
}
