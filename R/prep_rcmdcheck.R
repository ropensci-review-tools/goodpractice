
#' @include lists.R
#' @importFrom rcmdcheck rcmdcheck

PREPS$rcmdcheck <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  state$rcmdcheck <- rcmdcheck(path, quiet = quiet)
  state
}
