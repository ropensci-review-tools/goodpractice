#' @include lists.R prep_utils.R
#' @importFrom lintr lint_package

PREPS$tidyverse <- function(state, path = state$path, quiet) {
  if (is.null(state)) {
    state <- paste(
      "Check compliance with the Tidyverse style guide; mostly via 'lintr' package.",
      "(These checks are not run by default; and only if 'tidyverse_checks()' are",
      "added to 'checks_by_group()'."
    )
  } else {
    path <- normalizePath(path)
    excl <- as.list(state$exclude_path %||% character())
    state <- run_prep_step(state, "tidyverse_lintr", function(path) {
      suppressMessages(lint_package(path, exclusions = excl))
    }, path = path, silent = quiet)
  }
  state
}
