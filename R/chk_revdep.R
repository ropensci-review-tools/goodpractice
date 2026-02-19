
#' @include lists.R

query_reverse_deps <- function(pkg_name) {
  repos <- getOption("repos")["CRAN"]
  if (is.na(repos) || repos == "@CRAN@") {
    repos <- "https://cloud.r-project.org"
  }
  db <- utils::available.packages(repos = repos)
  deps <- tools::package_dependencies(pkg_name, db = db, reverse = TRUE)
  deps[[pkg_name]]
}

CHECKS$reverse_dependencies <- make_check(

  description = "Check for reverse dependencies on CRAN",
  tags = c("info", "CRAN"),
  preps = "description",

  gp = function(state) {
    res <- state$checks$reverse_dependencies
    revdeps <- vapply(res$positions, "[[", "", "filename")
    n <- length(revdeps)
    paste0(
      "run reverse dependency checks before CRAN submission. ",
      "This package has ", n, " reverse ",
      if (n == 1) "dependency" else "dependencies",
      " on CRAN: ",
      paste(utils::head(revdeps, 10), collapse = ", "),
      if (n > 10) paste0(", ... and ", n - 10, " more") else "",
      ". Use `revdepcheck::revdep_check()` to verify they still pass."
    )
  },

  check = function(state) {
    if (inherits(state$description, "try-error")) return(NA)

    pkg_name <- state$description$get_field("Package", default = NA_character_)
    if (is.na(pkg_name)) return(NA)

    revdeps <- tryCatch(
      query_reverse_deps(pkg_name),
      error = function(e) NA
    )

    if (identical(revdeps, NA)) return(NA)
    if (is.null(revdeps) || length(revdeps) == 0) return(TRUE)

    list(
      status = FALSE,
      positions = lapply(revdeps, function(dep) {
        list(
          filename = dep,
          line_number = NA_integer_,
          column_number = NA_integer_,
          ranges = list(),
          line = dep
        )
      })
    )
  }
)
