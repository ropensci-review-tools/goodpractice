
#' @include lists.R

PREPS$namespace <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  state$namespace <- parseNamespaceFile(
    basename(path),
    file.path(path, "..")
  )
  state
}
