
#' @noRd
#' @keywords internal
get_package_name <- function(path = ".") {
  state <- parseNamespaceFile(basename(path), file.path(path, ".."))
}

#' @noRd
#' @keywords internal
`%||%` <- function(l, r) { if (is.null(l)) r else l }

#' Default pattern for R files
#' @return Regular expression.
#' @noRd
#' @keywords internal

default_r_file_pattern <- function() {
  "\\.[RrSs]$"
}

#' @noRd
#' @keywords internal
trim_ws <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

#' @noRd
#' @keywords internal
loaded_pkg_version <- function(pkg) {
  asNamespace(pkg)$`.__NAMESPACE__.`$spec[["version"]]
}

#' @noRd
#' @keywords internal
drop_nulls <- function(l) {
  l[ ! vapply(l, is.null, TRUE) ]
}
