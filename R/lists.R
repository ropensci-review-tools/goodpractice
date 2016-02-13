
PREPS <- list()
CHECKS <- list()

all_checks <- function() {
  names(CHECKS)
}

make_check <- function(description, check, gp, ...) {

  structure(
    list(description = description, check = check, gp = gp, ...),
    class = "check"
  )
}
