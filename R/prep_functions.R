#' @include lists.R

#' Parse all top-level function definitions once for reuse by checks.
#'
#' Stores a list of function metadata (name, file, line, body) in
#' \code{state$functions} via \code{parse_package_functions()}.
#'
#' Used by:
#' \itemize{
#'   \item \code{chk_print_return.R}: \code{print_return_invisible}
#'   \item \code{chk_tidyverse.R}: \code{tidyverse_no_missing},
#'         \code{tidyverse_export_order}
#' }
PREPS$functions <- function(state, path = state$path, quiet) {
  state$functions <- parse_package_functions(path)
  state
}
