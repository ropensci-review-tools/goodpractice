
#' @include lists.R
#' @importFrom cyclocomp cyclocomp_package_dir

PREPS$cyclocomp <- function(state, path = state$path, quiet) {
  state$cyclocomp <- try({
    if (quiet) {
      withr::with_output_sink(nullfile(),
        withr::with_message_sink(nullfile(),
          cc <- cyclocomp_package_dir(path)
        )
      )
    } else {
      cc <- cyclocomp_package_dir(path)
    }

    pkg <- read.dcf(file.path(path, "DESCRIPTION"), "Package")[1, 1]
    ns <- asNamespace(pkg)
    is_fun <- vapply(cc$name, function(nm) {
      exists(nm, envir = ns, inherits = FALSE) &&
        is.function(get(nm, envir = ns))
    }, logical(1))
    cc[is_fun, ]
  }, silent = quiet)

  if (inherits(state$cyclocomp, "try-error")) {
    warning("Prep step for cyclomatic complexity failed.")
  }
  state
}
