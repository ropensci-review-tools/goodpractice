
#' Create a preparation function
#'
#' @param name Name of the preparation function.
#' @param func A function that takes two arguments:
#'   The \code{path} to the root directory of the package, and
#'   a logical argument: \code{quiet}. If \code{quiet} is true,
#'   the preparation function may print out diagnostic messages.
#'
#' @export

make_prep <- function(name, func) {
  force(name)
  force(func)
  function(state, path = state$path, quiet) {
    state[[name]] <- func(path, quiet)
    state
  }
}

#' @export

make_check <- function(description, check, gp, ...) {
  structure(
    list(description = description, check = check, gp = gp, ...),
    class = "check"
  )
}

prepare_preps <- function(preps, extra_preps) {
  utils::modifyList(preps, c(list(), extra_preps))
}

prepare_checks <- function(checks, extra_checks) {
  utils::modifyList(checks, c(list(), extra_checks))
}
