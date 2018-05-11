
#' @include lists.R
#' @importFrom desc description

PREPS$description <- function(state, path = state$path, quiet) {
  state$description <- try(description$new(file.path(path, "DESCRIPTION")), 
                           silent = quiet)
  if(inherits(state$description, "try-error")) {
    warning("Prep step for description failed.")
  }
  state
}
