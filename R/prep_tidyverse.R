#' @include lists.R
#' @importFrom lintr lint_package

PREPS$tidyverse <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  suppressMessages(
    state$tidyverse_lintr <- try(
      lint_package(path),
      silent = TRUE
    )
  )
  if (inherits(state$tidyverse_lintr, "try-error")) {
    cli::cli_warn("Prep step for {.val tidyverse_lintr} failed.")
  }
  state
}
