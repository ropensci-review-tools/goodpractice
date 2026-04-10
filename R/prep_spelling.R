#' @include lists.R
#' @importFrom spelling spell_check_package

PREPS$spelling <- function(state, path = state$path, quiet) {
  wordlist <- file.path(path, "inst", "WORDLIST")
  if (!file.exists(wordlist)) {
    state$spelling <- "no_wordlist"
    if (!quiet) {
      cli::cli_inform(
        "Skipping spelling check: no {.file inst/WORDLIST} found."
      )
    }
    return(state)
  }
  run_prep_step(state, "spelling", function(path) {
    suppressMessages(spell_check_package(path))
  }, path = path, silent = quiet)
}
