#' @include lists.R

#' Parse all top-level function definitions once for reuse by checks.
#'
#' Stores a list of function metadata (name, file, line, body) in
#' \code{state$functions} via \code{parse_package_functions()}.
#'
#' Used by:
#' \itemize{
#'   \item \code{chk_on_exit.R} (via \code{prep_on_exit.R}):
#'         \code{on_exit_add}, \code{on_exit_missing}
#' }
PREPS$functions <- function(state, path = state$path, quiet) {
  state$functions <- parse_package_functions(path)
  state
}
