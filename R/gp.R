
#' Run good practice checks
#'
#' To see the results, just print it to the screen.
#'
#' @param path Path to a package root.
#' @param checks Character vector, the checks to run. Defaults to
#'   all checks. Use \code{\link{all_checks}} to list all checks.
#' @param quiet Whether to suppress output from the preparation
#'   functions. Note that not all preparation functions produce output,
#'   even if this option is set to \code{FALSE}.
#' @return A goodpractice object that you can query
#'   with a simple API. See \code{\link{results}} to start.
#'
#' @export
#' @importFrom desc desc_get

gp <- function(path = ".", checks = all_checks(), quiet = TRUE) {

  preps <- unique(unlist(lapply(CHECKS[checks], "[[", "preps")))

  state <- list(
    path = path,
    package = desc_get("Package", file = file.path(path, "DESCRIPTION"))
  )

  for (prep in preps) {
    message("Preparing: ", prep)
    state <- PREPS[[prep]](state, quiet = quiet)
  }

  state$checks <- list()

  for (check in checks) {
    state$checks[[check]] <- CHECKS[[check]]$check(state)
  }

  class(state) <- "goodPractice"
  state
}

check_passed <- function(chk) {
  isTRUE(chk) || ("status" %in% names(chk) && isTRUE(chk[["status"]]))
}

check_failed <- function(chk) {
  ! check_passed(chk)
}
