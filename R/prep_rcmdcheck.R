
#' @include lists.R prep_utils.R
#' @importFrom rcmdcheck rcmdcheck

PREPS$rcmdcheck <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  run_prep_step(state, "rcmdcheck", function(path, quiet) {
    rcmdcheck(path, quiet = quiet)
  }, path = path, quiet = quiet, silent = quiet)
}
