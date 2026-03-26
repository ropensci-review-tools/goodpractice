#' Print goodpractice results
#'
#' @param x Object of class `goodPractice`, as returned by [gp()].
#' @param positions_limit How many positions to print at most.
#' @param ... Unused, for compatibility with [base::print()] generic method.
#'
#' @importFrom rstudioapi hasFun
#' @importFrom praise praise
#' @importFrom stats setNames
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
    cli::cat_rule(col = "yellow")
    if (failure &&
        getOption("goodpractice.rstudio_source_markers", TRUE) &&
        hasFun("sourceMarkers")) {
      rstudio_source_markers(x)
    }
  }

  if (!failure) {
    msg <- praise(
      '${Exclamation}! ${Adjective} package! Keep up the ${adjective} work!'
    )
    cli::cli_text("{cli::col_red(cli::symbol$heart)} {.strong {msg}}")
  }

  invisible(x)
}

#' @noRd
gp_header <- function(x) {
  cli::cat_rule(left = paste("GP", x$package), col = "yellow")
  cli::cli_text()
  cli::cli_text("{.strong It is good practice to}")
  cli::cli_text()
}

#' @noRd
gp_advice <- function(state, fail, limit, type = "error") {
  MYCHECKS <- prepare_checks(CHECKS, state$extra_checks)
  chk <- MYCHECKS[[fail]]
  res <- state$checks[[fail]]

  str <- if (is.function(chk$gp)) chk$gp(state) else chk$gp
  str <- cli::format_inline(gsub("\n\\s*", " ", str))
  str <- gsub("[{]", "{{", gsub("[}]", "}}", str))

  bullet <- switch(type, info = "i", warning = "!", "x")
  cli::cli_bullets(setNames(str, bullet))

  if ("positions" %in% names(res)) {
    cli::cli_text()
    withr::with_dir(state$path, gp_positions(res[["positions"]], limit))
  }

  cli::cli_text()
}

#' @noRd
gp_positions <- function(pos, limit) {
  num <- length(pos)
  if (num > limit) pos <- pos[seq_len(limit)]

  id <- cli::cli_div(theme = list(div = list("margin-left" = 4)))

  for (p in pos) {
    loc <- if (is.na(p$line_number)) "" else paste0(":", p$line_number)
    if (nzchar(loc) && !is.na(p$column_number)) {
      loc <- paste0(loc, ":", p$column_number)
    }
    cli::cli_text("{.path {p$filename}{loc}}")
  }

  if (num > limit) {
    cli::cli_text("{.emph ... and {num - limit} more line{?s}}")
  }

  cli::cli_end(id)
}
