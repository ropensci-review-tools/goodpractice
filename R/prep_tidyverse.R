#' @include lists.R prep_utils.R
#' @importFrom lintr lint_package

PREPS$tidyverse <- function(state, path = state$path, quiet) {
  if (is.null(state)) {
    state <- "Check compliance with the Tidyverse style guide; mostly via 'lintr' package."
  } else {
    path <- normalizePath(path)
    excl <- as.list(state$exclude_path %||% character())
    state <- run_prep_step(state, "tidyverse_lintr", function(path) {
      suppressMessages(lint_package(path, exclusions = excl))
    }, path = path, silent = quiet)
  }
  state
}
