
#' @export

checks <- function(gp) {
  names(gp$checks)
}

#' @export

results <- function(gp) {
  data.frame(
    stringsAsFactors = FALSE,
    row.names = NULL,
    check = names(gp$checks),
    result = vapply(x$checks, check_passed, TRUE)
  )
}

#' @export

failed_checks <- function(gp) {
  names(Filter(check_failed, gp$checks))
}

#' @export

failed_positions <- function(gp) {
  failed <- Filter(check_failed, gp$checks)
  lapply(failed, get_position)
}

get_position <- function(chk) {
  if (! "positions" %in% names(chk)) NULL else chk$positions
}
