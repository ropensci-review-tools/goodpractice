
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

#' @include lists.R
#' @importFrom lintr lint_package

read_lintr_linters <- function(config_path) {
  lines <- readLines(config_path, warn = FALSE)
  linter_line <- grep("^\\s*linters:", lines, value = TRUE)
  if (length(linter_line) == 0) return(NULL)
  expr_text <- sub("^\\s*linters:\\s*", "", linter_line[[1]])
  tryCatch(
    eval(parse(text = expr_text), envir = asNamespace("lintr")),
    error = function(e) NULL
  )
}

resolve_linters <- function(path) {
  lintr_config <- file.path(path, ".lintr")
  if (!file.exists(lintr_config)) return(linters_to_lint)

  cli::cli_inform("Using {.file .lintr} config from {.path {path}}")
  user_linters <- read_lintr_linters(lintr_config)
  if (is.null(user_linters)) return(linters_to_lint)

  modifyList(linters_to_lint, user_linters)
}

PREPS$lintr <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  linters <- resolve_linters(path)
  suppressMessages(
    state$lintr <- try(lint_package(path, linters = linters),
                       silent = TRUE)
  )
  if(inherits(state$lintr, "try-error")) {
    warning("Prep step for linter failed.")
  }
  state
}
