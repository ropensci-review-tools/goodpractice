
PREPS <- list()
CHECKS <- list()

#' List the names of all checks
#'
#' @return Character vector of checks
#' @export

all_checks <- function() {
  names(CHECKS)
}

#' List the names of default checks (excludes optional check sets)
#'
#' @return Character vector of default check names
#' @export

default_checks <- function() {
  setdiff(all_checks(), tidyverse_checks())
}

#' List the names of tidyverse style checks
#'
#' These checks are optional and not included in the default set.
#' They are powered by \code{\link[lintr]{lint_package}} using lintr's
#' default linter set and respect any \code{.lintr} configuration file
#' in the package root (e.g. to disable specific linters or add exclusions).
#' Add them via \code{checks = c(default_checks(), tidyverse_checks())}.
#'
#' @return Character vector of tidyverse check names
#' @export

tidyverse_checks <- function() {
  grep("^tidyverse_", all_checks(), value = TRUE)
}

#' Describe one or more checks
#'
#' @param check_name Names of checks to be described.
#' @return List of character descriptions for each \code{check_name}
#' @export
#' @examples 
#' describe_check("rcmdcheck_non_portable_makevars")
#' check_name <- c("no_description_depends",
#'                 "lintr_assignment_linter",
#'                 "no_import_package_as_a_whole",
#'                 "rcmdcheck_missing_docs")
#' describe_check(check_name)
#' # Or to see all checks:
#' \dontrun{
#'   describe_check(all_checks())
#' }

describe_check <- function(check_name = NULL) {
    check_name <- intersect(check_name, names(CHECKS))
    lapply(CHECKS[check_name], function(i) i$description)
}

#' List available prep names
#'
#' Returns the names of all registered preparation steps.
#' Use these names with [checks_by_prep()] to select checks by group,
#' or with \code{options(goodpractice.exclude_preps = ...)} to skip groups.
#'
#' @return Character vector of prep names
#' @export
#' @examples
#' all_preps()

all_preps <- function() {
  names(PREPS)
}

#' Select checks by prep group
#'
#' Returns the names of all checks that depend on the given prep(s).
#' This makes it easy to run or inspect a specific category of checks
#' without knowing individual check names.
#'
#' Call with no arguments to get checks that have no prep — these run
#' without any data gathering step.
#'
#' @param ... Prep names as character strings. Use [all_preps()] to see
#'   available names.
#' @return Character vector of check names
#' @export
#' @examples
#' # run only DESCRIPTION and namespace checks
#' checks_by_prep("description", "namespace")
#'
#' # see what the lintr prep covers
#' checks_by_prep("lintr")
#'
#' # find checks that need no prep
#' checks_by_prep()
#'
#' # use directly in gp()
#' \dontrun{
#'   gp(".", checks = checks_by_prep("description", "lintr"))
#' }

checks_by_prep <- function(...) {
  prep <- c(...)
  if (length(prep) == 0) {
    res <- names(Filter(function(ch) length(ch$preps) == 0, CHECKS))
    return(if (is.null(res)) character(0) else res)
  }

  res <- names(Filter(function(ch) any(ch$preps %in% prep), CHECKS))
  if (is.null(res)) character(0) else res
}
