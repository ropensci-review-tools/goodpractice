
#' @include lists.R prep_utils.R
#' @importFrom desc description

PREPS$description <- function(state, path = state$path, quiet) {
  run_prep_step(state, "description", function() {
    description$new(file.path(path, "DESCRIPTION"))
  }, quiet = quiet)
}
