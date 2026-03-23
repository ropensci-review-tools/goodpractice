
#' @include lists.R

PREPS$revdep <- function(state, path = state$path, quiet) {
  if (!curl::has_internet()) {
    state$revdep <- NA
    if (!quiet) {
      warning("Prep step for revdep skipped: no internet connection.")
    }
    return(state)
  }

  repos <- getOption("repos")["CRAN"]
  if (is.na(repos) || repos == "@CRAN@") {
    repos <- "https://cloud.r-project.org"
  }

  state$revdep <- try(
    utils::available.packages(repos = repos),
    silent = quiet
  )
  if (inherits(state$revdep, "try-error")) {
    warning("Prep step for revdep failed.")
  }
  state
}
