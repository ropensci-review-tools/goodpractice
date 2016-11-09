
#' @include lists.R
#' @importFrom cyclocomp cyclocomp_package_dir

PREPS$cyclocomp <- function(state, path = state$path, quiet) {
  state$cyclocomp <- cyclocomp_package_dir(path)
  state
}
