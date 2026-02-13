
PREPS <- list()
CHECKS <- list()

#' List the names of all checks
#'
#' @return Character vector of check names.
#' @export
#' @examples
#' all_checks()

all_checks <- function() {
  names(CHECKS)
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
#' \donttest{
#'   describe_check(all_checks())
#' }

describe_check <- function(check_name = NULL) {
    check_name <- intersect(check_name, names(CHECKS))
    lapply(CHECKS[check_name], function(i) i$description)
} 
