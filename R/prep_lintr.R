
linters_to_lint <- list(
  assignment_linter = lintr::assignment_linter,
  line_length_linter = lintr::line_length_linter(80)
)

#' @include lists.R
#' @importFrom lintr lint_package

PREPS$lintr <- function(state, path = state$path) {
  path <- normalizePath(path)
  state$lintr <- lint_package(path, linters = linters_to_lint)
  state
}
