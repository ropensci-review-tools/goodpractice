
#' Run good practice checks
#'
#' To see the results, just print it to the screen.
#'
#' @param path Path to a package root.
#' @param checks Character vector, the checks to run. Defaults to
#'   all checks. Use \code{\link{all_checks}} to list all checks.
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
#' @export
#' @aliases goodpractice
#' @importFrom desc desc_get
#' @examples 
#' path <- system.file("bad1", package = "goodpractice")
#' # run a subset of all checks available
#' g <- gp(path, checks = all_checks()[3:16])
#' g

gp <- function(path = ".", checks = all_checks(), extra_preps = NULL,
               extra_checks = NULL, quiet = TRUE) {

  MYPREPS <- prepare_preps(PREPS, extra_preps)
  MYCHECKS <- prepare_checks(CHECKS, extra_checks)

  preps <- unique(unlist(lapply(MYCHECKS[checks], "[[", "preps")))

  if(file.exists(file.path(path, "DESCRIPTION"))) {
    pkgname <- desc_get("Package", file = file.path(path, "DESCRIPTION"))
  } else {
    pkgname <- basename(normalizePath(path))
  }

  state <- list(
    path = path,
    package = pkgname,
    extra_preps = extra_preps,
    extra_checks = extra_checks
  )

  for (prep in preps) {
    message("Preparing: ", prep)
    state <- MYPREPS[[prep]](state, quiet = quiet)
  }

  state$checks <- list()

  for (check in checks) {
    state$checks[[check]] <- MYCHECKS[[check]]$check(state)
  }

  class(state) <- "goodPractice"
  state
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
  ! check_passed(chk, na_as_passed = na_as_passed)
}

#' @export goodpractice
goodpractice <- gp
