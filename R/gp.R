#' Run good practice checks
#'
#' To see the results, just print it to the screen.
#'
#' @param path Path to a package root.
#' @param checks Character vector, the checks to run. Defaults to
#'   \code{\link{default_checks}}. Use \code{\link{all_checks}} to list all
#'   checks, or add optional sets like \code{\link{tidyverse_checks}}.
#'   When \code{NULL}, all registered checks are run, subject to any
#'   exclusions from \code{goodpractice.exclude_check_groups} or
#'   \code{GP_EXCLUDE_CHECK_GROUPS}.
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
#' checks can be excluded by group name via the
#' \code{goodpractice.exclude_check_groups} option or the
#' \code{GP_EXCLUDE_CHECK_GROUPS} environment variable (comma-separated).
#' The option takes precedence.
#'
#' \preformatted{
#' # Skip URL and coverage checks:
#' options(goodpractice.exclude_check_groups = c("urlchecker", "covr"))
#'
#' # Or via environment variable:
#' Sys.setenv(GP_EXCLUDE_CHECK_GROUPS = "urlchecker,covr")
#' }
#'
#' Exclusion only applies when \code{checks = NULL} (the default).
#' Explicit \code{checks} arguments are never filtered.
#'
#' @section Excluding files:
#' Specific files can be excluded from checks via the
#' \code{goodpractice.exclude_path} option or the \code{GP_EXCLUDE_PATH}
#' environment variable (comma-separated). Paths are relative to the
#' package root.
#'
#' \preformatted{
#' options(goodpractice.exclude_path = c("R/RcppExports.R", "R/generated.R"))
#'
#' # Or via environment variable:
#' Sys.setenv(GP_EXCLUDE_PATH = "R/RcppExports.R,R/generated.R")
#' }
#'
#' Excluded files are skipped by lintr, treesitter, expression, and
#' roxygen2 checks.
#'
#' @section Parallel preparation:
#' Preparation steps run sequentially by default. To run them in
#' parallel, install \pkg{future.apply} and set a
#' \code{\link[future]{plan}}:
#'
#' \preformatted{
#' future::plan("multisession")
#' gp(".")
#' }
#'
#' Preps run in parallel only when a non-sequential plan is active.
#' Prep functions must be independent: in parallel mode each prep
#' receives the initial state snapshot, so a prep cannot read another
#' prep's output. Only new state fields are merged back; if two preps
#' write the same field, the second is dropped with a warning.
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
  checks = default_checks(),
  extra_preps = NULL,
  extra_checks = NULL,
  quiet = TRUE
) {
  pkgname <- validate_pkg_path(path)
  cli::cat_rule(
    paste("Preparing goodpractice for", pkgname),
    col = "cyan"
  )

  MYCHECKS <- prepare_checks(CHECKS, extra_checks)
  MYPREPS <- prepare_preps(PREPS, extra_preps)
  checks <- resolve_checks(checks, MYCHECKS)
  preps <- required_preps(checks, MYCHECKS)

  state <- init_state(path, pkgname, extra_preps, extra_checks) |>
    run_preps(preps, MYPREPS, quiet) |>
    run_checks(checks, MYCHECKS)

  state$.cache <- NULL
  class(state) <- "goodPractice"
  state
}

validate_pkg_path <- function(path) {
  if (!file.exists(file.path(path, "DESCRIPTION"))) {
    cli::cli_abort(c(
      "{.path {path}} must be a package.",
      i = "Can't find {.file DESCRIPTION}."
    ))
  }
  desc_get("Package", file = file.path(path, "DESCRIPTION"))
}

resolve_checks <- function(checks, mychecks) {
  if (is.null(checks)) {
    exclude_checks_by_group(names(mychecks), mychecks)
  } else {
    checks
  }
}

required_preps <- function(checks, mychecks) {
  unique(unlist(lapply(mychecks[checks], "[[", "preps")))
}

init_state <- function(path, pkgname, extra_preps, extra_checks) {
  list(
    path = path,
    package = pkgname,
    extra_preps = extra_preps,
    extra_checks = extra_checks,
    exclude_path = excluded_paths(),
    .cache = new.env(parent = emptyenv())
  )
}

run_preps <- function(state, preps, mypreps, quiet) {
  use_future <-
    requireNamespace("future.apply", quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")

  # nocov start - requires non-sequential future::plan()
  apply_fn <- if (use_future) {
    future.apply::future_lapply
  } else { # nocov end
    lapply
  }

  results <- apply_fn(preps, function(prep) {
    cli::cli_progress_step("Preparing: {prep}")
    result <- mypreps[[prep]](state, quiet = quiet)
    cli::cli_progress_done()
    result
  })

  # Preps must be independent: each receives the original state snapshot,

  # and only NEW fields (not already in state) are merged back. If two
  # preps write the same field, the second is dropped with a warning.
  for (res in results) {
    new_fields <- setdiff(names(res), names(state))
    conflict_fields <- intersect(names(res), names(state))
    init_fields <- c(
      "path", "package", "extra_preps",
      "extra_checks", "exclude_path", ".cache"
    )
    conflict_fields <- setdiff(conflict_fields, init_fields)
    if (length(conflict_fields) > 0) {
      cli::cli_warn(
        "Parallel prep conflict: field{?s} \\
        {.val {conflict_fields}} already set, skipping."
      )
    }
    for (field in new_fields) {
      state[[field]] <- res[[field]]
    }
  }

  state
}

run_checks <- function(state, checks, mychecks) {
  state$checks <- list()
  for (check in checks) {
    state$checks[[check]] <- mychecks[[check]]$check(state)
  }
  state
}

excluded_paths <- function() {
  opt <- getOption("goodpractice.exclude_path")
  if (!is.null(opt)) return(opt)
  env <- Sys.getenv("GP_EXCLUDE_PATH", "")
  parts <- strsplit(env, ",\\s*")[[1]]
  parts[nzchar(parts)]
}

excluded_check_groups <- function() {
  opt <- getOption("goodpractice.exclude_check_groups")
  if (!is.null(opt)) return(opt)
  env <- Sys.getenv("GP_EXCLUDE_CHECK_GROUPS", "")
  parts <- strsplit(env, ",\\s*")[[1]]
  parts[nzchar(parts)]
}

exclude_checks_by_group <- function(checks, mychecks) {
  exclude <- excluded_check_groups()
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

check_type <- function(chk) {
  if (is.list(chk) && "type" %in% names(chk)) chk$type else "error"
}

#' @export goodpractice
goodpractice <- gp
