
#' @include lists.R
#' @importFrom rcmdcheck rcmdcheck

PREPS$rcmdcheck <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  run_prep_step(state, "rcmdcheck", function() {
    rcmdcheck(path, quiet = quiet)
  }, quiet = quiet)
}
