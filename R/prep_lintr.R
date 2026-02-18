
linters_to_lint <- list(
  assignment_linter = lintr::assignment_linter(),
  line_length_linter = lintr::line_length_linter(80),
  package_hooks_linter = lintr::package_hooks_linter(),
  semicolon_linter = lintr::semicolon_linter(allow_compound = TRUE),
  attach_detach_linter = lintr::undesirable_function_linter(
    fun = lintr::default_undesirable_functions[c("attach", "detach")]
  ),
  setwd_linter = lintr::undesirable_function_linter(
    fun = lintr::default_undesirable_functions["setwd"]
  ),
  sapply_linter = lintr::undesirable_function_linter(
    fun = lintr::default_undesirable_functions["sapply"]
  ),
  library_require_linter = lintr::undesirable_function_linter(
    fun = lintr::default_undesirable_functions[c("library", "require")]
  ),
  seq_linter = lintr::seq_linter()
)

#' @include lists.R prep_utils.R
#' @importFrom lintr lint_package

PREPS$lintr <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  run_prep_step(state, "lintr", function() {
    suppressMessages(lint_package(path, linters = linters_to_lint))
  }, quiet = quiet)
}
