#' @include lists.R

rd_examples_info <- function(ex_section) {
  has_dontrun <- FALSE
  has_runnable_code <- FALSE

  for (el in ex_section) {
    tag <- attr(el, "Rd_tag")
    if (is.null(tag)) {
      next
    }

    if (tag == "\\dontrun") {
      has_dontrun <- TRUE
    } else if (tag == "RCODE") {
      code <- trimws(paste(unlist(el), collapse = ""))
      if (nzchar(code)) has_runnable_code <- TRUE
    }
  }

  list(has_dontrun = has_dontrun, has_runnable_code = has_runnable_code)
}

parse_rd_files <- function(mandir) {
  if (!dir.exists(mandir)) return(list())

  rd_files <- list.files(mandir, pattern = "\\.Rd$", full.names = TRUE)
  if (length(rd_files) == 0) return(list())

  lapply(rd_files, function(rd_file) {
    parsed <- tools::parse_Rd(rd_file)
    tags <- vapply(
      parsed,
      function(x) {
        tag <- attr(x, "Rd_tag")
        if (is.null(tag)) "" else tag
      },
      ""
    )

    aliases <- vapply(
      parsed[tags == "\\alias"],
      function(x) trimws(paste(unlist(x), collapse = "")),
      ""
    )

    has_examples <- any(tags == "\\examples")
    has_value <- any(tags == "\\value")

    ex_info <- list(has_dontrun = FALSE, has_runnable_code = FALSE)
    if (has_examples) {
      ex_info <- rd_examples_info(parsed[tags == "\\examples"][[1]])
    }

    list(
      file = basename(rd_file),
      aliases = aliases,
      has_examples = has_examples,
      has_value = has_value,
      has_dontrun = ex_info$has_dontrun,
      has_runnable_code = ex_info$has_runnable_code
    )
  })
}

PREPS$rd <- function(state, path = state$path, quiet) {
  mandir <- file.path(path, "man")

  state$rd <- try(parse_rd_files(mandir), silent = quiet)

  if (inherits(state$rd, "try-error")) {
    warning("Prep step for rd failed.")
  }
  state
}
