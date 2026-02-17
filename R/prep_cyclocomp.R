
#' @include lists.R
#' @importFrom cyclocomp cyclocomp_package_dir

PREPS$cyclocomp <- function(state, path = state$path, quiet) {
  state$cyclocomp <- try(cyclocomp_package_dir(path), silent = quiet)
  if(inherits(state$cyclocomp, "try-error")) {
    warning(
      "Prep step for cyclocomp failed: ",
      conditionMessage(attr(state$cyclocomp, "condition")),
      call. = FALSE
    )
  }
  state
}
