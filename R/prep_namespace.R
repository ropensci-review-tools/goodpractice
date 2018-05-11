
#' @include lists.R

PREPS$namespace <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  state$namespace <- try(parseNamespaceFile(
    basename(path),
    file.path(path, "..")
  ), silent = quiet)
  if(inherits(state$namespace, "try-error")) {
    warning("Prep step for namespace failed.")
  }
  state
}
