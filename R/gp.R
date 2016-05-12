
#' @export

gp <- function(path = ".", checks = all_checks()) {

  preps <- unique(unlist(lapply(CHECKS[checks], "[[", "preps")))

  state <- list(path = path)

  for (prep in names(PREPS)) {
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
