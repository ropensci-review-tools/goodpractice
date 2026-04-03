
PREPS <- list()
CHECKS <- list()

#' List the names of all checks
#'
#' @return Character vector of checks
#' @export
#' @examples
#' all_checks()

all_checks <- function() {
  names(CHECKS)
}

#' List the names of default checks (excludes optional check sets)
#'
#' @return Character vector of default check names
#' @export
#' @examples
#' default_checks()

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
#' @examples
#' tidyverse_checks()

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

#' List available check group names
#'
#' Returns the names of all registered check groups.
#' Use these names with [checks_by_group()] to select checks by group,
#' or with \code{options(goodpractice.exclude_check_groups = ...)} to skip
#' groups.
#'
#' @return Character vector of check group names
#' @export
#' @examples
#' all_check_groups()
#'
#' # See all checks by group
#' lapply(all_check_groups(), checks_by_group)

all_check_groups <- function() {
  names(PREPS)
}

#' Select checks by check group
#'
#' Returns the names of all checks that belong to the given group(s).
#' This makes it easy to run or inspect a specific category of checks
#' without knowing individual check names.
#'
#' @param ... Group names as character strings. Use [all_check_groups()] to
#'   see available names.
#' @return Character vector of check names
#' @export
#' @examples
#' # run only DESCRIPTION and namespace checks
#' checks_by_group("description", "namespace")
#'
#' # see what the lintr group covers
#' checks_by_group("lintr")
#' # See all checks by group:
#' lapply(all_check_groups(), checks_by_group)
#' # use directly in gp()
#' \dontrun{
#'   gp(".", checks = checks_by_group("description", "lintr"))
#' }

checks_by_group <- function(...) {
  group <- c(...)
  if (length(group) == 0) return(character(0))
  unknown <- setdiff(group, names(PREPS))
  if (length(unknown) > 0) {
    cli::cli_warn(c(
      "Unknown check group{?s}: {.val {unknown}}.",
      i = "Use {.fn all_check_groups} to see available groups."
    ))
  }
  names(Filter(function(ch) any(ch$preps %in% group), CHECKS))
}
