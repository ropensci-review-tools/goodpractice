#' @include lists.R
#' @importFrom spelling spell_check_package

PREPS$spelling <- function(state, path = state$path, quiet) {
  state$spelling <- try(
    suppressMessages(spell_check_package(path)),
    silent = quiet
  )
  if (inherits(state$spelling, "try-error")) {
    warning("Prep step for spelling failed.")
  }
  state
}
