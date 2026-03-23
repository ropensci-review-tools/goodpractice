
#' @include lists.R

PREPS$revdep <- function(state, path = state$path, quiet) {
  if (!curl::has_internet()) {
    state$available_packages <- NA
    if (!quiet) {
      warning("Prep step for available_packages skipped: no internet connection.")
    }
    return(state)
  }

  repos <- getOption("repos")["CRAN"]
  if (is.na(repos) || repos == "@CRAN@") {
    repos <- "https://cloud.r-project.org"
  }

  state$available_packages <- try(
    utils::available.packages(repos = repos),
    silent = quiet
  )
  if (inherits(state$available_packages, "try-error")) {
    warning("Prep step for available_packages failed.")
  }
  state
}
