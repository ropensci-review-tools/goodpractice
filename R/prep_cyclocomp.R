
#' @include lists.R
#' @importFrom cyclocomp cyclocomp_package_dir

PREPS$cyclocomp <- function(state, path = state$path, quiet) {
  if (quiet && requireNamespace("callr", quietly = TRUE)) {
    state$cyclocomp <- try(
      callr::r(
        function(path) cyclocomp::cyclocomp_package_dir(path),
        args = list(path = normalizePath(path)),
        show = FALSE
      ),
      silent = TRUE
    )
  } else {
    state$cyclocomp <- try(cyclocomp_package_dir(path), silent = quiet)
  }
  if (inherits(state$cyclocomp, "try-error")) {
    warning("Prep step for cyclomatic complexity failed.")
  }
  state
}
