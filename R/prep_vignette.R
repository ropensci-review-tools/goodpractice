
#' @include lists.R
#' @include chk_vignette.R
#' @include prep_utils.R

PREPS$vignette <- function(state, path = state$path, quiet) {
  if (is.null(state)) {
    state <- "Check that vignette code does not use either 'rm()' or 'setwd()'."
  } else {
    state <- run_prep_step(state, "vignette", function(path) {
      vfiles <- vignette_files(path)
      pd_list <- list()
      for (f in vfiles) {
        pd <- vignette_parse_data(f)
        if (!is.null(pd)) {
          pd_list[[f]] <- list(
            parse_data = pd,
            lines = readLines(f, warn = FALSE)
          )
        }
      }
      pd_list
    }, path = path, silent = quiet)
  }
  state
}
