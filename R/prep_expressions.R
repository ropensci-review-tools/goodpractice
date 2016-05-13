
#' `Collate` field from `DESCRIPTION`
#'
#' `NULL` is returned if there is no such field.
#'
#' @param path Path to the package root.
#' @return Character scalar or `NULL`.
#' @keywords internal
#'
#' @importFrom description desc_get_collate

package_collate <- function(path = ".") {
  col <- desc_get_collate(file = file.path(path, "DESCRIPTION"))
  if (length(col)) col else NULL
}

#' Get all source files of a package, in the right order
#'
#' It uses the `Collate` entry in the `DESCRIPTION` file,
#' if there is one. Otherwise the order is alphabetical.
#'
#' @param path Path to the root of the R package.
#' @return A character vector of (relative) file
#'   names in the corrent collation order.
#' @keywords internal

r_package_files <- function(path) {
  files <- package_collate(path)
  if (is.null(files)) {
    files <- list.files(
      file.path(path, "R"),
      pattern = default_r_file_pattern()
    )
  }

  file.path(path, "R", files)
}

#' Extract all closures from a package
#'
#' The package must be extracted into the working directory, as usual.
#'
#' We can use lintr to extract the functions, but need to use
#' our own code (based on similar code in functionMap) to
#' get the right collation order.
#' 
#' @param package Package name.
#' @param version Currently ignored.
#' @return The closures in a named list.
#'
#' @keywords internal
#'
#' @importFrom lintr get_source_expressions

prep_expressions <- function(package, version = NULL) {
  files <- lapply(
    r_package_files(package),
    get_source_expressions
  )
  expr <- lapply(files, "[[", "expressions")
  unlist(expr, recursive = FALSE)
}

#' @include lists.R

PREPS$expressions <- prep_expressions
