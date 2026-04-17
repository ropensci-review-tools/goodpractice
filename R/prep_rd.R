#' @include lists.R prep_utils.R

#' @noRd
parse_rd_files <- function(mandir) {
  if (!dir.exists(mandir)) {
    return(list())
  }

  rd_files <- list.files(mandir, pattern = "\\.Rd$", full.names = TRUE)
  if (length(rd_files) == 0) {
    return(list())
  }

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

    keywords <- vapply(
      parsed[tags == "\\keyword"],
      function(x) trimws(paste(unlist(x), collapse = "")),
      ""
    )

    doctypes <- vapply(
      parsed[tags == "\\docType"],
      function(x) trimws(paste(unlist(x), collapse = "")),
      ""
    )

    list(
      file = basename(rd_file),
      aliases = aliases,
      has_examples = any(tags == "\\examples"),
      has_value = any(tags == "\\value"),
      has_keyword_internal = "internal" %in% keywords,
      is_reexport = "import" %in% doctypes
    )
  })
}

PREPS$rd <- function(state, path = state$path, quiet) {
  run_prep_step(state, "rd", function(path) {
    parse_rd_files(file.path(path, "man"))
  }, path = path, silent = quiet)
}
