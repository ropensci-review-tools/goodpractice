
#' @include my_linters.R

linters_to_lint <- list(
  assignment_linter = lintr::assignment_linter(),
  line_length_linter = lintr::line_length_linter(80),
  trailing_semicolon_linter = trailing_semicolon_linter(),
  attach_detach_linter = lintr::undesirable_function_linter(fun = c(
    "attach" = "Avoid attach, it is easy to create errors with it",
    "detach" = "Avoid detach, it is easy to create errors with it"
  )),
  setwd_linter = lintr::undesirable_function_linter(fun = c(
    "setwd" = "Avoid changing the working directory, or restore it in on.exit"
  )),
  sapply_linter = lintr::undesirable_function_linter(fun = c(
    "sapply" = "Avoid using sapply, consider vapply instead, that's type safe"
  )),
  library_require_linter = lintr::undesirable_function_linter(fun = c(
    "library" = "Avoid library() calls in packages",
    "require" = "Avoid require() calls in packages"
  )),
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
