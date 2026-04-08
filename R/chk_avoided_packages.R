#' @include lists.R customization.R

AVOIDED_PACKAGES <- list(
  multicore = "Use the 'parallel' package instead.",
  RCurl     = "Use 'httr2', 'curl', or 'crul' instead.",
  rjson     = "Use 'jsonlite' instead.",
  RJSONIO   = "Use 'jsonlite' instead.",
  XML       = "Use 'xml2' instead.",
  sp        = "Use 'sf' instead. 'sp' is deprecated.",
  rgdal     = paste0(
    "Use 'sf', 'terra', or alternatives described at ",
    "https://www.hypertidy.org/posts/",
    "2026-03-24_check-out-wk/#check-it-out-series. ",
    "'rgdal' was retired in 2023."
  ),
  rgeos     = paste0(
    "Use 'sf', 'terra', or alternatives described at ",
    "https://www.hypertidy.org/posts/",
    "2026-03-24_check-out-wk/#check-it-out-series. ",
    "'rgeos' was retired in 2023."
  ),
  maptools  = "Use 'sf' instead. 'maptools' was retired in 2023."
)

CHECKS$no_obsolete_deps <- make_check(

  description = "No obsolete or retired package dependencies",
  tags = c("warning", "best practice"),
  preps = "description",

  gp = function(state) {
    if (inherits(state$description, "try-error")) {
      return("avoid depending on obsolete packages.")
    }
    deps <- state$description$get_deps()
    found <- intersect(deps$package, names(AVOIDED_PACKAGES))
    reasons <- vapply(found, function(p) {
      paste0(p, ": ", AVOIDED_PACKAGES[[p]])
    }, character(1))
    paste(
      "avoid depending on obsolete or retired packages.",
      paste(reasons, collapse = " ")
    )
  },

  check = function(state) {
    if (inherits(state$description, "try-error")) {
      return(na_result())
    }

    deps <- tryCatch(state$description$get_deps(), error = function(e) NULL)
    if (is.null(deps)) return(check_result(TRUE))

    found <- intersect(deps$package, names(AVOIDED_PACKAGES))
    if (length(found) == 0) return(check_result(TRUE))

    problems <- lapply(found, function(pkg) {
      dep_row <- deps[deps$package == pkg, ]
      check_position("DESCRIPTION", line = paste0(dep_row$type[1], ": ", pkg))
    })

    check_result(FALSE, problems)
  }
)
