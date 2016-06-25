
#' @export

print.goodPractice <- function(x, ...) {

  gp_header(x)

  for (check in names(x$checks)) {
    if (! check_passed(x$checks[[check]])) gp_advice(x, check)
  }

  gp_footer(x)

  invisible(x)
}

#' @importFrom clisymbols symbol

make_line <- function(x) {
  paste(rep(symbol$line, x), collapse = "")
}

lines <- vapply(1:100, FUN.VALUE = "", make_line)

header_line <- function(left = "", right = "",
                        width = getOption("width")) {

  ncl <- nchar(left)
  ncr <- nchar(right)

  if (ncl) left <- paste0(" ", left, " ")
  if (ncr) right <- paste0(" ", right, " ")
  ndashes <- width - ((ncl > 0) * 2  + (ncr > 0) * 2 + ncl + ncr)

  if (ndashes < 4) {
    right <- substr(right, 1, ncr - (4 - ndashes))
    ncr <- nchar(right)

  }

  dashes <- if (ndashes <= length(lines)) {
    lines[ndashes]
  } else {
    make_line(ndashes)
  }

  res <- paste0(
    substr(dashes, 1, 2),
    left,
    substr(dashes, 3, ndashes - 4),
    right,
    substr(dashes, ndashes - 3, ndashes)
  )[1]

  substring(res, 1, width)
}

gp_header <- function(x) {
  h <- header_line(left = paste("GP", x$package))
  cat(h, "\n\n", sep = "")
  cat("It is good practice to\n\n")
}

gp_footer <- function(x) {
  f <- header_line()
  cat(f, "\n")
}

#' @importFrom clisymbols symbol

gp_advice <- function(state, fail) {

  chk <- CHECKS[[fail]]
  res <- state$checks[[fail]]

  str <- if (is.function(chk$gp)) chk$gp(state) else chk$gp

  str <- gsub("\n\\s*", " ", str)
  str <- paste(
    strwrap(paste0(symbol$cross, " ", str), indent = 2, exdent = 4),
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
