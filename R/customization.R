
#' Create a preparation function
#'
#' @param name Name of the preparation function.
#' @param func A function that takes two arguments:
#'   The \code{path} to the root directory of the package, and
#'   a logical argument: \code{quiet}. If \code{quiet} is true,
#'   the preparation function may print out diagnostic messages.
#'
#' @export
#' @examples 
#' # make a preparation function
#' url_prep <- make_prep(
#'   name = "desc", 
#'   func = function(path, quiet) desc::description$new(path)
#' )
#' # and the corresponding check function
#' url_chk <- make_check(
#'   description = "URL field in DESCRIPTION",
#'   tags = character(),
#'   preps = "desc",
#'   gp = "have a URL field in DESCRIPTION",
#'   check = function(state) state$desc$has_fields("URL")
#' )
#' # use together in gp()
#' bad1 <- system.file("bad1", package = "goodpractice")
#' res <- gp(bad1, checks = "no_description_depends",
#'           extra_preps = list("desc" = url_prep),
#'           extra_checks = list("url" = url_chk))

make_prep <- function(name, func) {
  force(name)
  force(func)
  function(state, path = state$path, quiet) {
    state[[name]] <- func(path, quiet)
    state
  }
}

#' Create a check function
#' 
#' @param description A description of the check.
#' @param check A function that takes the \code{state} as an argument.
#' @param gp A short description of what is good practice.
#' @param ... Further arguments.
#' @export
#' @examples 
#' # make a preparation function
#' url_prep <- make_prep(
#'   name = "desc", 
#'   func = function(path, quiet) desc::description$new(path)
#' )
#' # and the corresponding check function
#' url_chk <- make_check(
#'   description = "URL field in DESCRIPTION",
#'   tags = character(),
#'   preps = "desc",
#'   gp = "have a URL field in DESCRIPTION",
#'   check = function(state) state$desc$has_fields("URL")
#' )
#' # use together in gp()
#' bad1 <- system.file("bad1", package = "goodpractice")
#' res <- gp(bad1, checks = "no_description_depends",
#'           extra_preps = list("desc" = url_prep),
#'           extra_checks = list("url" = url_chk))

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
