#' @include lists.R
#' @importFrom urlchecker url_check

has_internet <- function() {
  curl::has_internet()
}

run_url_check <- function(path, quiet) {
  if (!has_internet()) {
    stop("No internet connection available.")
  }
  urlchecker::url_check(path, progress = !quiet)
}

PREPS$urlchecker <- function(state, path = state$path, quiet) {
  state$urlchecker <- try(run_url_check(path, quiet), silent = quiet)

  if (inherits(state$urlchecker, "try-error")) {
    warning("Prep step for urlchecker failed.")
  }
  state
}
