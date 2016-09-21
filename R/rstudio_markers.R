
#' @importFrom rstudioapi callFun

rstudio_source_markers <- function(gp) {

  markers <- get_markers(gp)

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

#' @param gp `gp()` output
#' @param check name of the check to extract
#'
#' @keywords internal

get_marker <- function(gp, check) {

  chk <- CHECKS[[check]]
  res <- gp$checks[[check]]

  ## Check passed
  if (check_passed(gp$checks[[check]])) return(NULL)

  ## Check failed, but alas, no positions
  if (! "positions" %in% names(gp$checks[[check]])) return(NULL)

  my_message <- if (is.function(chk$gp)) chk$gp(gp) else chk$gp

  lapply(
    res$positions,
    function(p) {
      list(
        type = "warning",                # TODO
        file = normalizePath(file.path(gp$path, p$filename)),
        line = if (is.na(p$line_number)) 1L else p$line_number,
        column = if (is.na(p$column_number)) 1L else p$column_number,
        message = my_message
      )
    }
  )
}
