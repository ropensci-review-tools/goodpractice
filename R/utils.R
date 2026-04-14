
#' @noRd
`%||%` <- function(l, r) { if (is.null(l)) r else l }

#' @noRd
check_result <- function(status = NULL, positions = NULL, ...) {
  list(
    status = status %||% NA,
    positions = positions %||% list(),
    ...
  )
}

na_result <- function() {
  check_result()
}

#' Construct a source position object
#'
#' General constructor for the position objects returned by check
#' functions. Each position identifies a location in the source code
#' where a check found an issue. Use this instead of building
#' `list(filename, line_number, ...)` by hand.
#'
#' @param filename Path relative to the package root, e.g. `"R/foo.R"`.
#' @param line_number Line number (1-based), or `NA_integer_` if unknown.
#' @param column_number Column number, or `NA_integer_` if unknown.
#' @param ranges List of start/end pairs for multi-line issues.
#' @param line Short display string identifying the problem to the user.
#' @return A named list with the five position fields.
#' @keywords internal
#' @noRd
check_position <- function(filename, line_number = NA_integer_,
                           column_number = NA_integer_,
                           ranges = list(), line = "") {
  list(
    filename = filename,
    line_number = line_number,
    column_number = column_number,
    ranges = ranges,
    line = line
  )
}

ns_s3_method_names <- function(ns) {
  s3m <- ns$S3methods
  if (is.null(s3m) || nrow(s3m) == 0) return(character())
  paste0(s3m[, 1], ".", s3m[, 2])
}

#' Default pattern for R files
#' @return Regular expression.
#' @keywords internal
#' @noRd
default_r_file_pattern <- function() {
  "\\.[RrSs]$"
}

filter_excluded_paths <- function(files, pkg_path, exclude_path) {
  if (length(exclude_path) == 0) return(files)
  abs_excluded <- normalizePath(file.path(pkg_path, exclude_path),
                                mustWork = FALSE)
  abs_files <- normalizePath(files, mustWork = FALSE)
  files[!abs_files %in% abs_excluded]
}

read_source_file <- function(path, encoding = "UTF-8") {
  con <- file(path, encoding = encoding)
  on.exit(close(con))
  paste(readLines(con, warn = FALSE), collapse = "\n")
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

has_internet <- function() {
  curl::has_internet()
}

# Used only to parse code chunks in vignettes;
# all other code is parsed with treesitter.R fns
safe_parse <- function(file = NULL, text = NULL, keep_source = TRUE,
                       encoding = "UTF-8") {
  args <- if (!is.null(file)) {
    list(file = file, keep.source = keep_source, encoding = encoding)
  } else {
    list(text = text, keep.source = keep_source)
  }

  tryCatch(
    do.call(base::parse, args),
    error = function(e) {
      if (keep_source) {
        args$keep.source <- FALSE
        tryCatch(do.call(base::parse, args), error = function(e) NULL)
      } else {
        NULL
      }
    }
  )
}
