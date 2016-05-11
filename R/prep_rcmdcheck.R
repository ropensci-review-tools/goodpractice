
#' @include lists.R

PREPS$rcmdcheck <- function(state, path = state$path) {
  path <- normalizePath(path)
  state$rcmdcheck <- rcmdcheck::rcmdcheck(path, quiet = TRUE)
  state
}
