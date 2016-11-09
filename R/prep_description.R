
#' @include lists.R
#' @importFrom desc description

PREPS$description <- function(state, path = state$path, quiet) {
  state$description <- description$new(file.path(path, "DESCRIPTION"))
  state
}
