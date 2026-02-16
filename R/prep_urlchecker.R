#' @include lists.R
#' @importFrom urlchecker url_check

PREPS$urlchecker <- function(state, path = state$path, quiet) {
  state$urlchecker <- try(
    urlchecker::url_check(path, progress = !quiet),
    silent = quiet
  )

  if (inherits(state$urlchecker, "try-error")) {
    warning("Prep step for urlchecker failed.")
  }
  state
}
