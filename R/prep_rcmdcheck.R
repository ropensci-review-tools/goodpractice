
#' @include lists.R
#' @importFrom rcmdcheck rcmdcheck

PREPS$rcmdcheck <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  state$rcmdcheck <- try(rcmdcheck(path, quiet = quiet), silent = quiet)
  if(inherits(state$rcmdcheck, "try-error")) {
    warning("Prep step for rcmdcheck failed.")
  }
  state
}
