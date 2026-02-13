#' An exported function
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
confused_func <- function() {
  2
}
