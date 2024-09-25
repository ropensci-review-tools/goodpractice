
#' @importFrom lintr Lint

trailing_semicolon_linter <- function() lintr::Linter(function(source_file) {

  allsc <- which(source_file$parsed_content$token == "';'")

  if (!length(allsc)) return(list())

  ## Check that it is the last token in the line
  ## Note that we need to restrict the search to terminal
  ## nodes, because parse is buggy and sometimes reports false,
  ## too large end positions for non-terminal nodes.
  badsc <- Filter(
    x = allsc,
    f = function(line) {
      with(
        source_file$parsed_content,
        all(! terminal | line1 != line1[line] | col2 <= col2[line])
      )
    }
  )

  lapply(
    badsc,
    function(line) {
      parsed <- source_file$parsed_content[line, ]
      Lint(
        filename = source_file$filename,
        line_number = parsed$line1,
        column_number = parsed$col1,
        type = "style",
        message = "Avoid trailing semicolons, they are not needed.",
        line = source_file$lines[as.character(parsed$line1)],
        ranges = list(c(parsed$col1, parsed$col2))
      )
    }
  )
})

#' @importFrom lintr Lint

dangerous_functions_linter <- function(source_file, funcs, type,
                                       msg, linter) lintr::Linter(function(source_file) {

  bad <- which(
    source_file$parsed_content$token == "SYMBOL_FUNCTION_CALL" &
    source_file$parsed_content$text %in% funcs
  )

  lapply(
    bad,
    function(line) {
      parsed <- source_file$parsed_content[line, ]
      Lint(
        filename = source_file$filename,
        line_number = parsed$line1,
        column_number = parsed$col1,
        type = type,
        message = msg,
        line = source_file$lines[as.character(parsed$line1)],
        ranges = list(c(parsed$col1, parsed$col2))
      )
    }
  )
})

attach_detach_linter <- function(source_file) {
  dangerous_functions_linter(
    source_file,
    funcs = c("attach", "detach"),
    type = "warning",
    msg = "Avoid attach/detach, it is easy to create errors with it"
  )
}

setwd_linter <- function(source_file) {
  dangerous_functions_linter(
    source_file,
    funcs = "setwd",
    type = "warning",
    msg = "Avoid changing the working directory, or restore it in on.exit"
  )
}

sapply_linter <- function(source_file) {
  dangerous_functions_linter(
    source_file,
    funcs = "sapply",
    type = "warning",
    msg = "Avoid using sapply, consider vapply instead, that's type safe"
  )
}

library_require_linter <- function(source_file) {
  dangerous_functions_linter(
    source_file,
    funcs = c("library", "require"),
    type = "warning",
    msg = "Avoid library() and require() calls in packages"
  )
}
