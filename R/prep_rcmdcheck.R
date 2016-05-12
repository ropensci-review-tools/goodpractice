
#' @include lists.R
#' @importFrom rcmdcheck rcmdcheck

PREPS$rcmdcheck <- function(state, path = state$path) {
  path <- normalizePath(path)
  state$rcmdcheck <- rcmdcheck(path, quiet = TRUE)
  state
}
