#' Run good practice checks
#'
#' To see the results, just print it to the screen.
#'
#' @param path Path to a package root.
#' @param checks Character vector, the checks to run. When \code{NULL}
#'   (the default), all registered checks are run, subject to any
#'   exclusions from \code{goodpractice.exclude_preps} or
#'   \code{GP_EXCLUDE_PREPS}. Explicitly passing check names overrides
#'   any exclusion settings. Use \code{\link{all_checks}} to list
#'   available checks.
#' @param extra_preps Custom preparation functions. See
#'   \code{\link{make_prep}} on creating preparation functions.
#' @param extra_checks Custom checks. See \code{\link{make_check}} on
#'   creating checks.
#' @param quiet Whether to suppress output from the preparation
#'   functions. Note that not all preparation functions produce output,
#'   even if this option is set to \code{FALSE}.
#' @return A goodpractice object that you can query
#'   with a simple API. See \code{\link{results}} to start.
#'
#' @section Excluding check groups:
#' When using the default \code{checks = all_checks()}, entire groups of
#' checks can be excluded by prep name via the
#' \code{goodpractice.exclude_preps} option or the \code{GP_EXCLUDE_PREPS}
#' environment variable (comma-separated). The option takes precedence.
#'
#' \preformatted{
#' # Skip URL and coverage checks:
#' options(goodpractice.exclude_preps = c("urlchecker", "covr"))
#'
#' # Or via environment variable:
#' Sys.setenv(GP_EXCLUDE_PREPS = "urlchecker,covr")
#' }
#'
#' Exclusion only applies when \code{checks = NULL} (the default).
#' Explicit \code{checks} arguments are never filtered.
#'
#' @export
#' @aliases goodpractice
#' @importFrom desc desc_get
#' @examples
#' path <- system.file("bad1", package = "goodpractice")
#' # run a subset of all checks available
#' g <- gp(path, checks = all_checks()[3:16])
#' g

gp <- function(
  path = ".",
  checks = NULL,
  extra_preps = NULL,
  extra_checks = NULL,
  quiet = TRUE
) {

  MYPREPS <- prepare_preps(PREPS, extra_preps)
  MYCHECKS <- prepare_checks(CHECKS, extra_checks)

  if (is.null(checks)) {
    checks <- exclude_checks_by_prep(names(MYCHECKS), MYCHECKS)
  }

  preps <- unique(unlist(lapply(MYCHECKS[checks], "[[", "preps")))

  if (file.exists(file.path(path, "DESCRIPTION"))) {
    pkgname <- desc_get("Package", file = file.path(path, "DESCRIPTION"))
  } else {
    cli::cli_abort(c(
      "{.path path} must be a package.",
      i = "Can't find DESCRIPTION."
    ))
  }

  state <- list(
    path = path,
    package = pkgname,
    extra_preps = extra_preps,
    extra_checks = extra_checks
  )

  for (prep in preps) {
    cli::cli_progress_step("Preparing: {prep}")
    state <- MYPREPS[[prep]](state, quiet = quiet)
    cli::cli_progress_done()
  }

  state$checks <- list()

  for (check in checks) {
    state$checks[[check]] <- MYCHECKS[[check]]$check(state)
  }

  class(state) <- "goodPractice"
  state
}

excluded_preps <- function() {
  opt <- getOption("goodpractice.exclude_preps")
  if (!is.null(opt)) return(opt)
  env <- Sys.getenv("GP_EXCLUDE_PREPS", "")
  parts <- strsplit(env, ",\\s*")[[1]]
  parts[nzchar(parts)]
}

exclude_checks_by_prep <- function(checks, mychecks) {
  exclude <- excluded_preps()
  if (length(exclude) == 0) return(checks)

  dominated <- vapply(checks, function(ch) {
    any(mychecks[[ch]]$preps %in% exclude)
  }, logical(1))

  if (any(dominated)) {
    cli::cli_warn("Excluding checks that depend on: {.val {exclude}}")
  }
  checks[!dominated]
}

check_passed <- function(chk, na_as_passed = FALSE) {
  status <- if ("status" %in% names(chk)) {
    chk$status
  } else {
    chk
  }

  if (na_as_passed) {
    isTRUE(status) || is.na(status)
  } else if (is.na(status)) {
    NA
  } else {
    isTRUE(status)
  }
}

check_failed <- function(chk, na_as_passed = FALSE) {
  !check_passed(chk, na_as_passed = na_as_passed)
}

#' @export goodpractice
goodpractice <- gp
