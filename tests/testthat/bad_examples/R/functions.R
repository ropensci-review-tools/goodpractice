#' Uses dontrun unnecessarily
#'
#' @examples
#' x <- 1 + 1
#' \dontrun{
#'   uses_dontrun()
#' }
#' @export
uses_dontrun <- function() {
  TRUE
}

#' All example code is non-runnable
#'
#' @examples
#' \donttest{
#'   all_wrapped()
#' }
#' @export
all_wrapped <- function() {
  FALSE
}

#' Proper examples
#'
#' @examples
#' good_example()
#' @export
good_example <- function() {
  NULL
}

#' Function with consecutive examples blocks
#'
#' @examples
#' x <- 1
#' @examples
#' \dontrun{
#'   multi_block()
#' }
#' @export
multi_block <- function() {
  TRUE
}
