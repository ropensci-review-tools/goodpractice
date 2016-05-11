
#' @export

print.goodPractice <- function(x, ...) {

  gp_header()

  for (check in names(x$checks)) {
    if (! x$checks[[check]]) gp_advice(x, CHECKS[[check]]$gp)
  }

  invisible(x)
}

gp_header <- function() {
  cat("--------------------------------------------------------------\n")
  cat("It is good practice to\n\n")
}

gp_advice <- function(state, str) {
  if (is.function(str)) str <- str(state)
  str <- gsub("\n\\s*", " ", str)
  str <- paste(
    strwrap(paste0("* ", str), indent = 2, exdent = 4),
    collapse = "\n"
  )
  cat(str, "\n\n")
}
