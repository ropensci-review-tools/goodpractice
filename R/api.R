
#' List all checks performed
#'
#' @param gp \code{\link{gp}} output.
#' @return  Character vector of check names.
#'
#' @family API
#' @export
#' @examples 
#' path <- system.file("bad1", package = "goodpractice")
#' # Run a subset of all checks available
#' g <- gp(path, checks = all_checks()[9:16])
#' checks(g)
#' # Or run with named check groups
#' g <- gp(path, checks = checks_by_group("description", "namespace"))
#' checks(g)

checks <- function(gp) {
  names(gp$checks)
}

#' Return all check results in a data frame
#'
#' @param gp \code{\link{gp}} output.
#' @return Data frame, with columns:
#' \item{check}{The name of the check.}
#' \item{passed}{Logical, whether the check passed.}
#'
#' @family API
#' @export
#' @examples 
#' path <- system.file("bad1", package = "goodpractice")
#' # Run a subset of all checks available
#' g <- gp(path, checks = all_checks()[9:16])
#' results(g)
#' # Or run with named check groups
#' g <- gp(path, checks = checks_by_group("description", "namespace"))
#' results(g)

results <- function(gp) {
  data.frame(
    row.names = NULL,
    check = names(gp$checks),
    passed = vapply(gp$checks, check_passed, TRUE)
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
#' g <- gp(path, checks = all_checks()[9:16])
#' failed_checks(g)
#' # Or run with named check groups
#' g <- gp(path, checks = checks_by_group("description", "namespace"))
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
#' @examples
#' path <- system.file("bad1", package = "goodpractice")
#' g <- gp(path, checks = "description_url")
#' failed_positions(g)

failed_positions <- function(gp) {
  failed <- Filter(check_failed, gp$checks)
  lapply(failed, get_position)
}

get_position <- function(chk) {
  if ("positions" %in% names(chk)) chk$positions else NULL
}

#' Export failed checks to JSON
#'
#' @param gp \code{\link{gp}} output.
#' @param file Output connection or file.
#' @param pretty Whether to pretty-print the JSON.
#' @return Invisibly returns the path to the output file.
#'
#' @export
#' @examples
#' path <- system.file("bad1", package = "goodpractice")
#' g <- gp(path, checks = "description_url")
#' tmp <- tempfile(fileext = ".json")
#' export_json(g, tmp)
#' unlink(tmp)
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
  invisible(file)
}

#' Download a local copy of 'goodpractice4agents.md' to tell AI agents how to
#' use this package.
#'
#' The 'goodpractice4agents.md' file contains sufficient instructions for most
#' AI %agents to edit package code so that it passes most 'goodpractice'
#' checks. The file can be directly edited and tweaked for personal use cases.
#' An agent can be instructed to simply "run the file goodpractice4agents.md".
#' The file can also be moved to any local agent's \code{skills/} directory to
#' call with a "skills" command. Alternatively, the file can be directly
#' downloaded from GitHub at
#' \url{https://github.com/ropensci-review-tools/goodpractice/tree/main/inst/skills/goodpractice4agents.md}.
#'
#' @param path Local path where file should be extracted.
#' @return (Invisibly) Full path to file.
#' \code{FALSE}.
#'
#' @export
#' @examples
#' path <- file.path(tempdir(), "skill.md")
#' f <- write_gp4agents(path)
#' file.exists(f)

write_gp4agents <- function(path = ".") {
    if (file.exists(path)) {
        stop("File ", path, " already exists.", call. = FALSE)
    }
    if (dir.exists(path)) {
        path <- file.path(path, "goodpractice4agents.md")
    } else {
        if (!dir.exists(dirname(path))) {
            stop(
                "Directory ",
                basename(path),
                " does not exist.",
                call. = FALSE
            )
        }
    }

    src <- system.file(
        "skills", "goodpractice4agents.md", package = "goodpractice"
    )
    if (!file.exists(src)) {
        stop("Expected error: File ", src, " not found.", .call = FALSE)
    }
    file.copy(src, path)

    return(path)
}
