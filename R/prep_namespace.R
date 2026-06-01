
#' @include lists.R prep_utils.R

PREPS$namespace <- function(state, path = state$path, quiet) {
  if (is.null(state)) {
    state <- "Check import and export patterns in NAMESPACE file"
  } else {
    path <- normalizePath(path)
    state <- run_prep_step(state, "namespace", function(path) {
      parseNamespaceFile(basename(path), file.path(path, ".."))
    }, path = path, silent = quiet)
  }
  state
}
