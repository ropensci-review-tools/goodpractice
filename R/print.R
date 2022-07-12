#' Print goodpractice results
#'
#' @param x Object of class `goodPractice`, as returned by [gp()].
#' @param positions_limit How many positions to print at most.
#' @param ... Unused, for compatibility with [base::print()] generic method.
#'
#' @importFrom rstudioapi hasFun
#' @importFrom praise praise
#' @importFrom clisymbols symbol
#' @importFrom crayon red bold
#'
#' @export

print.goodPractice <- function(x, positions_limit = 5, ...) {

  failure <- FALSE

  for (check in names(x$checks)) {
    if (! check_passed(x$checks[[check]], na_as_passed = TRUE)) {
      if (!failure) {
        failure <- TRUE
        gp_header(x)
      }
      gp_advice(x, check, positions_limit)
    }
  }

  if (failure) {
    gp_footer(x)
    if (getOption("goodpractice.rstudio_source_markers", TRUE) &&
        hasFun("sourceMarkers")) {
      rstudio_source_markers(x)
    }

  } else {
    cat(
      "\n", sep = "",
      bold(red(clisymbols::symbol$heart)),
      praise(paste0(
        " ${Exclamation}! ${Adjective} package! ",
        "Keep up the ${adjective} work!"
      )),
      "\n"
    )
  }

  invisible(x)
}

#' @importFrom clisymbols symbol

make_line <- function(x) {
  paste(rep(clisymbols::symbol$line, x), collapse = "")
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
  cat(crayon::yellow(h), "\n\n", sep = "")
  cat(crayon::bold("It is good practice to"), "\n\n", sep = "")
}

gp_footer <- function(x) {
  f <- header_line()
  cat(crayon::yellow(f), "\n")
}

#' @importFrom clisymbols symbol

gp_advice <- function(state, fail, limit) {

  MYCHECKS <- prepare_checks(CHECKS, state$extra_checks)

  chk <- MYCHECKS[[fail]]
  res <- state$checks[[fail]]

  str <- if (is.function(chk$gp)) chk$gp(state) else chk$gp

  str <- gsub("\n\\s*", " ", str)
  str <- paste(
    strwrap(
      paste0(crayon::red(clisymbols::symbol$cross), " ", str),
      indent = 2,
      exdent = 4
    ),
    collapse = "\n"
  )

  cat(str)

  if ("positions" %in% names(res)) gp_positions(res[["positions"]], limit)

  cat("\n")
}

gp_positions <- function(pos, limit) {

  num <- length(pos)
  if (length(pos) > limit) pos <- pos[1:limit]

  cat("\n\n")
  lapply(pos, function(x) {
    cat(sep = "", "    ", crayon::blue(x$filename), ":",
        crayon::blue(as.character(x$line_number)), ":",
        crayon::blue(as.character(x$column_number)), "\n")
  })

  if (num > limit) {
    and <- paste0("    ... and ", num - limit, " more lines\n")
    cat(crayon::blue(and))
  }
}
