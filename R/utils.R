
get_package_name <- function(path = ".") {
  state <- parseNamespaceFile(basename(path), file.path(path, ".."))
}

`%||%` <- function(l, r) { if (is.null(l)) r else l }

#' Default pattern for R files
#' @return Regular expression.
#' @keywords internal

default_r_file_pattern <- function() {
  "\\.[RrSs]$"
}

trim_ws <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

loaded_pkg_version <- function(pkg) {
  asNamespace(pkg)$`.__NAMESPACE__.`$spec[["version"]]
}

drop_nulls <- function(l) {
  l[ ! vapply(l, is.null, TRUE) ]
}
