
#' @include lists.R prep_utils.R
#' @importFrom cyclocomp cyclocomp_package_dir

PREPS$cyclocomp <- function(state, path = state$path, quiet) {
  run_prep_step(state, "cyclocomp", function(path) {
    df <- cyclocomp_package_dir(path)
    r_files <- list.files(
      file.path(path, "R"),
      pattern = "\\.[rR]$",
      full.names = TRUE
    )
    top_level_fns <- unlist(lapply(r_files, function(f) {
      lines <- readLines(f, warn = FALSE)
      pat <- "^(?:`([^`]+)`|([a-zA-Z._][a-zA-Z0-9._]*))\\s*(<-|=)\\s*function\\s*\\("
      m <- regmatches(lines, regexec(pat, lines, perl = TRUE))
      vapply(m[lengths(m) > 0], function(x) {
        if (nzchar(x[2])) x[2] else x[3]
      }, "")
    }))
    df[df$name %in% top_level_fns, , drop = FALSE]
  }, path = path, silent = quiet)
}
