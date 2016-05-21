
PREPS <- list()
CHECKS <- list()

#' List the names of all checks
#'
#' @return Character vector of checks
#' @export

all_checks <- function() {
  names(CHECKS)
}

make_check <- function(description, check, gp, ...) {

  structure(
    list(description = description, check = check, gp = gp, ...),
    class = "check"
  )
}
