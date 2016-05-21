
#' Run good practice checks
#'
#' To see the results, just print it to the screen.
#'
#' @param path Path to a package root.
#' @param checks Character vector, the checks to run. Defaults to
#'   all checks. Use \code{\link{all_checks}} to list all checks.
#' @return A goodpractice object that you can query
#'   with a simple API. See \code{\link{results}} to start.
#'
#' @export

gp <- function(path = ".", checks = all_checks()) {

  preps <- unique(unlist(lapply(CHECKS[checks], "[[", "preps")))

  state <- list(path = path)

  for (prep in preps) {
    message("Preparing: ", prep)
    state <- PREPS[[prep]](state)
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
