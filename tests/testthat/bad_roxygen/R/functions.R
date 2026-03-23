#' An exported function
#' @return TRUE
#' @export
exported_func <- function() {
  TRUE
}

#' Internal with noRd but missing keywords internal
#' @noRd
internal_no_keywords <- function() {
  FALSE
}

#' Properly tagged internal function
#' @noRd
#' @keywords internal
internal_proper <- function() {
  NULL
}

untagged_func <- function() {
  1
}

#' Contradictory tags
#' @export
#' @keywords internal
#' @return A number.
confused_func <- function() {
  2
}

#' Uses a deprecated tag
#' @S3method print foo
#' @noRd
#' @keywords internal
deprecated_tag_func <- function() {
  3
}

#' Inherits from nonexistent function
#' @inheritParams nonexistent_function
#' @return Nothing.
#' @export
#' @keywords internal
bad_inherit_func <- function(x) {
  x
}
