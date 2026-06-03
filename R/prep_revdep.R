
#' @include lists.R

PREPS$revdep <- function(state, path = state$path, quiet) {
  if (!curl::has_internet()) {
    state$revdep <- NA
    if (!quiet) {
      cli::cli_warn(
        "Prep step for {.val revdep} skipped: no internet connection."
      )
    }
    return(state)
  }

  repos <- getOption("repos")["CRAN"]
  if (is.na(repos) || repos == "@CRAN@") {
    repos <- "https://cloud.r-project.org"
  }

  run_prep_step(state, "revdep", function(repos) {
    utils::available.packages(repos = repos)
  }, repos = repos, silent = quiet)
}
