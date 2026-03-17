#' Print goodpractice results
#'
#' @param x Object of class `goodPractice`, as returned by [gp()].
#' @param positions_limit How many positions to print at most.
#' @param ... Unused, for compatibility with [base::print()] generic method.
#'
#' @importFrom rstudioapi hasFun
#' @importFrom praise praise
#' @importFrom cli symbol col_red style_bold
#'
#' @export

print.goodPractice <- function(x, positions_limit = 5, ...) {

  failure <- FALSE
  has_info <- FALSE

  for (check in names(x$checks)) {
    type <- check_type(x$checks[[check]])
    if (!check_passed(x$checks[[check]], na_as_passed = TRUE)) {
      if (!failure) {
        failure <- TRUE
        gp_header(x)
      }
      gp_advice(x, check, positions_limit, type = "error")
    } else if (type == "info") {
      if (!failure && !has_info) {
        has_info <- TRUE
        gp_header(x)
      }
      gp_advice(x, check, positions_limit, type = "info")
    }
  }

  if (failure || has_info) {
    gp_footer()
    if (failure &&
        getOption("goodpractice.rstudio_source_markers", TRUE) &&
        hasFun("sourceMarkers")) {
      rstudio_source_markers(x)
    }

  }

  if (!failure) {
    cat(
      "\n", sep = "",
      style_bold(col_red(cli::symbol$heart)),
      praise(paste0(
        " ${Exclamation}! ${Adjective} package! ",
        "Keep up the ${adjective} work!"
      )),
      "\n"
    )
  }

  invisible(x)
}

gp_header <- function(x) {
  cli::cat_rule(
    left = paste("GP", x$package),
    col = "yellow"
  )
  cat("\n", cli::style_bold("It is good practice to"), "\n\n", sep = "")
}

gp_footer <- function() {
  cli::cat_rule(col = "yellow")
}

#' @importFrom cli symbol

gp_advice <- function(state, fail, limit, type = "error") {

  MYCHECKS <- prepare_checks(CHECKS, state$extra_checks)

  chk <- MYCHECKS[[fail]]
  res <- state$checks[[fail]]

  str <- if (is.function(chk$gp)) chk$gp(state) else chk$gp

  str <- gsub("\n\\s*", " ", str)

  icon <- switch(type,
    info    = cli::col_blue(cli::symbol$info),
    warning = cli::col_yellow("!"),
    cli::col_red(cli::symbol$cross)
  )

  str <- paste(
    cli::ansi_strwrap(
      paste0(icon, " ", str),
      indent = 2,
      exdent = 4
    ),
    collapse = "\n"
  )

  cat(str)

  if ("positions" %in% names(res)) {
    withr::with_dir(
      state$path,
      gp_positions(res[["positions"]], limit)
    )
  }

  cat("\n")
}

gp_positions <- function(pos, limit) {

  num <- length(pos)
  if (length(pos) > limit) pos <- pos[1:limit]

  cat("\n\n")
  lapply(pos, function(x) {
    # Only display line and column when available
    if (is.na(x$line_number)) {
      # In the unlikely scenario that only column is available
      # it would still be unusable without line_number
      pos <- ""
    } else {
      pos <- paste0(":", x$line_number)
      if (!is.na(x$column_number)) {
        pos <- paste0(pos, ":", x$column_number)
      }
    }
    cat(sep = "", "    ", 
        cli::format_inline("{.path {x$filename}{pos}}"),
        "\n")
  })

  if (num > limit) {
    and <- paste0("    ... and ", num - limit, " more lines\n")
    cat(cli::col_blue(and))
  }
}
