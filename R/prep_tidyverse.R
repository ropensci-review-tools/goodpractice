#' @include lists.R prep_utils.R
#' @importFrom lintr lint_package

PREPS$tidyverse <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  excl <- as.list(state$exclude_path %||% character())
  run_prep_step(state, "tidyverse_lintr", function(path) {
    suppressMessages(lint_package(path, exclusions = excl))
  }, path = path, silent = quiet)
}
