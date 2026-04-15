
#' @include lists.R prep_utils.R
#' @importFrom desc description

PREPS$description <- function(state, path = state$path, quiet) {
  run_prep_step(state, "description", function(path) {
    desc <- description$new(file.path(path, "DESCRIPTION"))
    if (!desc$has_fields("Encoding")) {
      desc$set("Encoding", "UTF-8")
    }
    desc
  }, path = path, silent = quiet)
}
