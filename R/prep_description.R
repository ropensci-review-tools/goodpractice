
#' @include lists.R prep_utils.R
#' @importFrom desc description

PREPS$description <- function(state, path = state$path, quiet) {
  if (is.null(state)) {
    state <- paste0(
      "Check common issues with DESCRIPTION files, including ",
      "formatting issues with URLs, DOIs, package names, and ",
      "author and contributor roles"
    )
  } else {
    state <- run_prep_step(state, "description", function(path) {
      desc <- description$new(file.path(path, "DESCRIPTION"))
      if (!desc$has_fields("Encoding")) {
        desc$set("Encoding", "UTF-8")
      }
      desc
    }, path = path, silent = quiet)
  }
  state
}
