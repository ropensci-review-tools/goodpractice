#' Write a `.lintr` file into a test fixture directory
#'
#' lintr falls back to a user's global `~/.lintr` whenever a project has no
#' `.lintr` of its own, so a developer's personal lintr config can silently
#' change which tidyverse/lintr checks pass or fail on these fixtures. This
#' function sive every fixture package its own `.lintr` so tests are isolated
#' from that.
#'
#' (`.lintr` files can't be committed to the fixture packages themselves
#' because CRAN forbids all dotfiles.)
#'
#' @param path Directory of the fixture package (or temp dir) to write into.
#' @param linters Text of the `linters:` value in the generated `.lintr`
#'   file; defaults to lintr's own defaults.
#' @return Invisibly, the path of the `.lintr` file written.
write_dot_lintr <- function(path, linters = "linters_with_defaults()") {
  lintr_path <- file.path(path, ".lintr")
  writeLines(paste0("linters: ", linters), lintr_path)
  invisible(lintr_path)
}
