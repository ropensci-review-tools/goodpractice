
#' @include lists.R
#' @importFrom description description

PREPS$description <- function(state, path = ".") {
  state$description <- description$new(file.path(path, "DESCRIPTION"))
  state
}
