
#' @include lists.R
#' @importFrom cyclocomp cyclocomp_package_dir

PREPS$cyclocomp <- function(state, path = state$path, quiet) {
  run_prep_step(state, "cyclocomp", function() {
    cyclocomp_package_dir(path)
  }, quiet = quiet)
}
