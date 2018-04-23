
#' @importFrom rstudioapi callFun

rstudio_source_markers <- function(gp) {

  markers <- get_markers(gp)

  if (length(markers) == 0) return()

  callFun(
    "sourceMarkers",
    name = "goodpractice",
    markers = markers,
    basePath = NULL,
    autoSelect = "first"
  )
}

get_markers <- function(gp) {
  m <- lapply(names(gp$checks), get_marker, gp = gp)
  m <- drop_nulls(m)
  unlist(m, recursive = FALSE)
}

#' Get a marker from the positions of a check
#'
#' @param gp `gp()` output
#' @param check name of the check to extract
#'
#' @keywords internal
#' @importFrom utils head

get_marker <- function(gp, check) {

  chk <- CHECKS[[check]]
  res <- gp$checks[[check]]

  ## Check passed
  if (check_passed(gp$checks[[check]], na_as_passed = TRUE)) return(NULL)

  ## Check failed, but alas, no positions
  if (! "positions" %in% names(gp$checks[[check]])) return(NULL)

  my_message <- if (is.function(chk$gp)) chk$gp(gp) else chk$gp

  type <- head(intersect(
    chk$tags,
    c("error", "warning", "info", "style", "usage")
  ), 1)

  lapply(
    res$positions,
    function(p) {
      list(
        type = type,
        file = normalizePath(file.path(gp$path, p$filename)),
        line = if (is.na(p$line_number)) 1L else p$line_number,
        column = if (is.na(p$column_number)) 1L else p$column_number,
        message = my_message
      )
    }
  )
}
