
#' @include lists.R prep_utils.R

PREPS$namespace <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  run_prep_step(state, "namespace", function(path) {
    parseNamespaceFile(basename(path), file.path(path, ".."))
  }, path = path, silent = quiet)
}
