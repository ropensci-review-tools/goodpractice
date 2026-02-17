
#' @include lists.R
#' @importFrom cyclocomp cyclocomp_package_dir

PREPS$cyclocomp <- function(state, path = state$path, quiet) {
  state$cyclocomp <- try(cyclocomp_package_dir(path, quiet = quiet))
  if(inherits(state$cyclocomp, "try-error")) {
    warning("Prep step for cyclomatic complexity failed.")
  }
  state
}
