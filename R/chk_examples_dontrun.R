#' @include lists.R

#' @noRd
#' @keywords internal
extract_examples_blocks <- function(path) {
  blocks <- list()
  rfiles <- r_package_files(path)

  for (f in rfiles) {
    if (!file.exists(f)) next
    lines <- readLines(f, warn = FALSE)
    in_examples <- FALSE
    example_start <- NA_integer_
    example_lines <- character()

    for (i in seq_along(lines)) {
      ln <- lines[i]

      if (!grepl("^\\s*#'", ln)) {
        if (in_examples && length(example_lines) > 0) {
          blocks[[length(blocks) + 1]] <- list(
            filename = file.path("R", basename(f)),
            start = example_start,
            lines = example_lines
          )
        }
        in_examples <- FALSE
        example_lines <- character()
        next
      }

      content <- sub("^\\s*#'\\s?", "", ln)

      if (grepl("^@examples\\b", content)) {
        if (in_examples && length(example_lines) > 0) {
          blocks[[length(blocks) + 1]] <- list(
            filename = file.path("R", basename(f)),
            start = example_start,
            lines = example_lines
          )
        }
        in_examples <- TRUE
        example_start <- i
        example_lines <- character()
        next
      }

      if (grepl("^@", content)) {
        if (in_examples && length(example_lines) > 0) {
          blocks[[length(blocks) + 1]] <- list(
            filename = file.path("R", basename(f)),
            start = example_start,
            lines = example_lines
          )
        }
        in_examples <- FALSE
        example_lines <- character()
        next
      }

      if (in_examples) {
        example_lines <- c(example_lines, content)
      }
    }
  }

  blocks
}

CHECKS$examples_dontrun <- make_check(

  description = "Examples do not use \\dontrun",
  tags = c("documentation", "CRAN"),
  preps = character(),

  gp = paste(
    "Replace \\dontrun{} with \\donttest{} in examples.",
    "\\dontrun{} should only be used if the example truly cannot be",
    "executed by the user (e.g. missing API keys or external software)."
  ),

  check = function(state) {
    blocks <- extract_examples_blocks(state$path)
    problems <- list()

    for (block in blocks) {
      if (any(grepl("\\\\dontrun\\s*\\{", block$lines))) {
        problems[[length(problems) + 1]] <- list(
          filename = block$filename,
          line_number = block$start,
          column_number = NA_integer_,
          ranges = list(),
          line = paste0("#' @examples (line ", block$start, ")")
        )
      }
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)

CHECKS$examples_all_nonrunnable <- make_check(

  description = "Examples have runnable code outside \\dontrun/\\donttest",
  tags = c("documentation", "CRAN"),
  preps = character(),

  gp = paste(
    "Include some directly runnable example code.",
    "Wrapping all example code in \\dontrun{} or \\donttest{} means",
    "nothing is checked by R CMD check. At least some examples should",
    "run unconditionally."
  ),

  check = function(state) {
    blocks <- extract_examples_blocks(state$path)
    problems <- list()

    for (block in blocks) {
      code <- paste(block$lines, collapse = "\n")
      stripped <- code
      stripped <- gsub(
        "\\\\dont(run|test)\\s*\\{[^}]*\\}", "", stripped
      )
      stripped <- gsub("\\s", "", stripped)

      if (nzchar(code) && !nzchar(stripped)) {
        problems[[length(problems) + 1]] <- list(
          filename = block$filename,
          line_number = block$start,
          column_number = NA_integer_,
          ranges = list(),
          line = paste0("#' @examples (line ", block$start, ")")
        )
      }
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)
