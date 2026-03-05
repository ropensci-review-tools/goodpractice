#' @include lists.R
#' @importFrom urlchecker url_check

run_url_check <- function(path, quiet) {
  urlchecker::url_check(path, progress = !quiet)
}

PREPS$urlchecker <- function(state, path = state$path, quiet) {
  if (!has_internet()) {
    warning("Skipping URL checks: no internet connection.")
    state$urlchecker <- try(stop("offline"), silent = TRUE)
    return(state)
  }

  state$urlchecker <- try(run_url_check(path, quiet), silent = quiet)
  if (inherits(state$urlchecker, "try-error")) {
    warning("Prep step for urlchecker failed.")
  }
  state
}
