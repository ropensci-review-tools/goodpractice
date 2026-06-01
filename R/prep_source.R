#' @include lists.R

# Sentinel preps: checks that perform their own work (via ts_get() or
# direct filesystem calls) still need a registered prep name so that
# required_preps() can resolve the check's `preps` field.

PREPS$code_structure <- function(state, path = state$path, quiet) {
  if (is.null(state)) {
    state <- paste0(
      "Common issues like functions which are duplicated, or ",
      "not actually used."
    )
  }
  state
}

PREPS$package_structure <- function(state, path = state$path, quiet) {
  if (is.null(state)) {
    state <- paste0(
      "Generic checks like whether a package has a README, a NEWS file, ",
      "or whether all files use a '.R' extension, and not '.r'."
    )
  }
  state
}
