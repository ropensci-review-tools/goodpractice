
#' List all checks performed
#'
#' @param gp \code{\link{gp}} output.
#' @return  Character vector of check names.
#'
#' @family API
#' @export
#' @examples 
#' path <- system.file("bad1", package = "goodpractice")
#' # run a subset of all checks available
#' g <- gp(path, checks = all_checks()[3:16])
#' checks(g)

checks <- function(gp) {
  names(gp$checks)
}

#' Return all check results in a data frame
#'
#' @param gp \code{\link{gp}} output.
#' @return Data frame, with columns:
#' \item{check}{The name of the check.}
#' \item{result}{Logical, whether it has failed or not.}
#'
#' @family API
#' @export
#' @examples 
#' path <- system.file("bad1", package = "goodpractice")
#' # run a subset of all checks available
#' g <- gp(path, checks = all_checks()[3:16])
#' results(g)

results <- function(gp) {
  data.frame(
    stringsAsFactors = FALSE,
    row.names = NULL,
    check = names(gp$checks),
    result = vapply(gp$checks, check_passed, TRUE)
  )
}

#' Names of the failed checks
#'
#' @param gp \code{\link{gp}} output.
#' @return Names of the failed checks.
#'
#' @family API
#' @export
#' @examples 
#' path <- system.file("bad1", package = "goodpractice")
#' # run a subset of all checks available
#' g <- gp(path, checks = all_checks()[3:16])
#' failed_checks(g)

failed_checks <- function(gp) {
  names(Filter(check_failed, gp$checks))
}

#' Positions of check failures in the source code
#'
#' Note that not all checks refer to the source code.
#' For these the result will be \code{NULL}.
#'
#' For the ones that do, the results is a list, one for each failure.
#' Since the same check can fail multiple times. A single failure
#' is a list with entries: \code{filename}, \code{line_number},
#' \code{column_number}, \code{ranges}. \code{ranges} is a list of
#' pairs of start and end positions for each line involved in the
#' check.
#'
#' @param gp \code{\link{gp}} output.
#' @return A list of lists of positions. See details below.
#'
#' @export

failed_positions <- function(gp) {
  failed <- Filter(check_failed, gp$checks)
  lapply(failed, get_position)
}

get_position <- function(chk) {
  if (! "positions" %in% names(chk)) NULL else chk$positions
}

#' Export failed checks to JSON
#'
#' @param gp \code{\link{gp}} output.
#' @param file Output connection or file.
#' @param pretty Whether to pretty-print the JSON.
#'
#' @export
#' @importFrom jsonlite toJSON
#' @importFrom whoami username fullname

export_json <- function(gp, file, pretty = FALSE) {

  obj <- list(
    package = gp$description$get("Package"),
    path = gp$path,
    failures = Filter(check_failed, gp$checks),
    gp_version = loaded_pkg_version("goodpractice"),
    date = as.character(Sys.time()),
    user = username(fallback = "<unknown>"),
    name = fullname(fallback = "<unknown>")
  )

  cat(toJSON(obj, pretty = pretty), file = file)
  invisible()
}
