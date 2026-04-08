
#' @include lists.R

#' @noRd
query_reverse_deps <- function(pkg_name, db) {
  deps <- tools::package_dependencies(pkg_name, db = db, reverse = TRUE)
  deps[[pkg_name]]
}

revdep_gp_message <- function(revdeps) {
  n <- length(revdeps)
  dep_list <- paste(utils::head(revdeps, 10), collapse = ", ")
  suffix <- ifelse(n > 10, paste0(", ... and ", n - 10L, " more"), "")
  paste0(
    "run revdepcheck::revdep_check() before CRAN submission. ",
    "This package has ", n, " reverse ",
    ifelse(n == 1, "dependency", "dependencies"),
    " on CRAN: ", dep_list, suffix, "."
  )
}

CHECKS$reverse_dependencies <- make_check(

  description = "Check for reverse dependencies on CRAN",
  tags = c("info", "CRAN"),
  preps = c("description", "revdep"),

  gp = function(state) {
    revdeps <- state$checks$reverse_dependencies$revdeps
    revdep_gp_message(revdeps)
  },

  check = function(state) {
    if (inherits(state$description, "try-error")) return(na_result())
    if (identical(state$revdep, NA) ||
        inherits(state$revdep, "try-error")) return(na_result())

    pkg_name <- state$description$get_field("Package", default = NA_character_)
    if (is.na(pkg_name)) return(na_result())

    revdeps <- tryCatch(
      query_reverse_deps(pkg_name, state$revdep),
      error = function(e) NA
    )

    if (identical(revdeps, NA)) return(na_result())
    if (is.null(revdeps) || length(revdeps) == 0) {
      return(check_result(TRUE))
    }

    check_result(TRUE, type = "info", revdeps = revdeps)
  }
)
