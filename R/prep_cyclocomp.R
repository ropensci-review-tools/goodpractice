
#' @include lists.R prep_utils.R
#' @importFrom cyclocomp cyclocomp_package_dir

PREPS$cyclocomp <- function(state, path = state$path, quiet) {
  run_prep_step(state, "cyclocomp", function(path) {
    cyclocomp_package_dir(path)
  }, path = path, silent = quiet)
}
