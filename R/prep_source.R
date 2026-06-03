#' @include lists.R

# Sentinel preps: checks that perform their own work (via ts_get() or
# direct filesystem calls) still need a registered prep name so that
# required_preps() can resolve the check's `preps` field.

PREPS$code_structure <- function(state, path = state$path, quiet) {
  state
}

PREPS$package_structure <- function(state, path = state$path, quiet) {
  state
}
