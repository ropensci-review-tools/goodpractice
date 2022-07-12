
#' @include my_linters.R

linters_to_lint <- list(
  assignment_linter = lintr::assignment_linter(),
  line_length_linter = lintr::line_length_linter(80),
  trailing_semicolon_linter = trailing_semicolon_linter(),
  attach_detach_linter = attach_detach_linter(),
  setwd_linter = setwd_linter(),
  sapply_linter = sapply_linter(),
  library_require_linter = library_require_linter(),
  seq_linter = seq_linter()
)

#' @include lists.R
#' @importFrom lintr lint_package

PREPS$lintr <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  suppressMessages(
    state$lintr <- try(lint_package(path, linters = linters_to_lint),
                       silent = TRUE)
  )
  if(inherits(state$lintr, "try-error")) {
    warning("Prep step for linter failed.")
  }
  state
}
