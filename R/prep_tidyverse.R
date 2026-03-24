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
    warning("Prep step for tidyverse_lintr failed.")
  }
  state
}
