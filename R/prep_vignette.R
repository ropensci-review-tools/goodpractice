
#' @include lists.R
#' @include chk_vignette.R

PREPS$vignette <- function(state, path = state$path, quiet) {
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
  state$vignette <- pd_list
  state
}
