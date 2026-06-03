#' @include lists.R

# Sentinel preps: checks that perform their own work (via ts_get() or
# direct filesystem calls) still need a registered prep name so that
# required_preps() can resolve the check's `preps` field.

PREPS$code_structure <- function(state, path = state$path, quiet) {
  if (is.null(state)) {
    state <- paste(
      "Common issues like duplicated or unused function bodies,",
      "that 'print()' returns insivibly, that 'on.exit()' uses",
      "'add = TRUE', and checks on function length (default max",
      "50 lines)."
    )
  }
  state
}

PREPS$package_structure <- function(state, path = state$path, quiet) {
  if (is.null(state)) {
    state <- paste(
      "Generic checks like whether a package has a README, a NEWS file,",
      "or whether all files use a '.R' extension, and not '.r'."
    )
  }
  state
}
