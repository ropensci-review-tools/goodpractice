
#' @export

print.goodPractice <- function(x, ...) {

  gp_header()

  for (check in names(x$checks)) {
    if (! check_passed(x$checks[[check]])) gp_advice(x, check)
  }

  invisible(x)
}

gp_header <- function() {
  cat("--------------------------------------------------------------\n")
  cat("It is good practice to\n\n")
}

gp_advice <- function(state, fail) {

  chk <- CHECKS[[fail]]
  res <- state$checks[[fail]]

  str <- if (is.function(chk$gp)) chk$gp(state) else chk$gp

  str <- gsub("\n\\s*", " ", str)
  str <- paste(
    strwrap(paste0("* ", str), indent = 2, exdent = 4),
    collapse = "\n"
  )

  cat(str)

  if ("positions" %in% names(res)) gp_positions(res[["positions"]])

  cat("\n")
}

gp_positions <- function(pos, limit = 5) {

  num <- length(pos)
  if (length(pos) > limit) pos <- pos[1:5]

  cat("\n\n")
  lapply(pos, function(x) {
    cat(sep = "", "    ", x$filename, ":", x$line_number, ":",
        x$column_number, "\n")
  })

  if (num > limit) cat("    ... and ", num - 5, " more lines\n")
}
