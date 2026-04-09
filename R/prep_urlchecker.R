#' @include lists.R
#' @importFrom urlchecker url_check

#' @noRd
run_url_check <- function(path, quiet) {
  urlchecker::url_check(path, progress = !quiet)
}

PREPS$urlchecker <- function(state, path = state$path, quiet) {
  if (!has_internet()) {
    cli::cli_warn("Skipping URL checks: no internet connection.")
    state$urlchecker <- try(stop("offline"), silent = TRUE)
    return(state)
  }

  run_prep_step(state, "urlchecker", function(path, quiet) {
    run_url_check(path, quiet)
  }, path = path, quiet = quiet, silent = quiet)
}
