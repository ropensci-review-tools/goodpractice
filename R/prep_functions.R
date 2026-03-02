#' @include lists.R

PREPS$functions <- function(state, path = state$path, quiet) {
  state$functions <- parse_package_functions(path)
  state
}
