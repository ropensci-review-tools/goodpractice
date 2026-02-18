#' @include lists.R
#' @importFrom utils getFromNamespace

PREPS$spelling <- function(state, path = state$path, quiet) {
  spell_check <- getFromNamespace("spell_check_package", "spelling")
  state$spelling <- try(
    suppressMessages(spell_check(path)),
    silent = quiet
  )
  if (inherits(state$spelling, "try-error")) {
    warning("Prep step for spelling failed.")
  }
  state
}
