#' @include lists.R
#' @importFrom spelling spell_check_package

PREPS$spelling <- function(state, path = state$path, quiet) {
  wordlist <- file.path(path, "inst", "WORDLIST")
  if (!file.exists(wordlist)) {
    state$spelling <- "no_wordlist"
    if (!quiet) cli::cli_inform("Skipping spelling check: no {.file inst/WORDLIST} found.")
    return(state)
  }
  state$spelling <- try(
    suppressMessages(spell_check_package(path)),
    silent = quiet
  )
  if (inherits(state$spelling, "try-error")) {
    cli::cli_warn("Prep step for {.val spelling} failed.")
  }
  state
}
