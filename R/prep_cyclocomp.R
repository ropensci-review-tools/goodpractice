
#' @include lists.R
#' @importFrom cyclocomp cyclocomp_package_dir

PREPS$cyclocomp <- function(state, path = state$path, quiet) {
  if (quiet && requireNamespace("callr", quietly = TRUE)) {
    state$cyclocomp <- try(
      callr::r(
        function(path) {
          cc <- cyclocomp::cyclocomp_package_dir(path)
          pkg <- basename(path)
          ns <- asNamespace(pkg)
          is_fun <- vapply(cc$name, function(nm) {
            exists(nm, envir = ns, inherits = FALSE) &&
              is.function(get(nm, envir = ns))
          }, logical(1))
          cc[is_fun, ]
        },
        args = list(path = normalizePath(path)),
        show = FALSE
      ),
      silent = TRUE
    )
  } else {
    state$cyclocomp <- try(
      cyclocomp_package_dir(path),
      silent = quiet
    )
    if (!inherits(state$cyclocomp, "try-error")) {
      pkg <- basename(normalizePath(path))
      ns <- asNamespace(pkg)
      is_fun <- vapply(state$cyclocomp$name, function(nm) {
        exists(nm, envir = ns, inherits = FALSE) &&
          is.function(get(nm, envir = ns))
      }, logical(1))
      state$cyclocomp <- state$cyclocomp[is_fun, ]
    }
  }
  if (inherits(state$cyclocomp, "try-error")) {
    warning("Prep step for cyclomatic complexity failed.")
  }
  state
}
