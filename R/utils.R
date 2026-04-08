
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
