
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

#' Find dangerous 1:x expressions
#'
#' Find occurrences of \code{1:length(x)}, \code{1:nrow(x)},
#' \code{1:ncol(x)}, \code{1:NROW(x)}, \code{1:NCOL(x)} where
#' \code{x} is an R expression.
#'
#' @return Lint object.
#'
#' @importFrom xmlparsedata xml_parse_data
#' @importFrom xml2 read_xml xml_find_all xml_text xml_children xml_attr
#' @keywords internal

seq_linter <- function() lintr::Linter(function(source_file) {

  if (!length(source_file$parsed_content)) return(list())

  xml <- if ('xml_parsed_content' %in% names(source_file)) {
    source_file$xml_parsed_content
  } else {
    read_xml(xml_parse_data(source_file$parsed_content))
  }

  bad_funcs <- c("length", "nrow", "ncol", "NROW", "NCOL")
  text_clause <- paste0("text() = '", bad_funcs, "'", collapse = " or ")

  xpath <- paste0(
    "//expr",
    "[expr[NUM_CONST[text()='1' or text()='1L']]]",
    "[OP-COLON]",
    "[expr[expr[SYMBOL_FUNCTION_CALL[", text_clause, "]]]]"
  )

  badx <- xml_find_all(xml, xpath)

  ## The actual order of the nodes is document order
  ## In practice we need to handle length(x):1
  get_fun <- function(x, n) {
    funcall <- xml_children(xml_children(x)[[n]])
    if (!length(funcall)) return(NULL)
    fun <- trim_ws(xml_text(funcall[[1]]))
    if (! fun %in% bad_funcs) fun else paste0(fun, "(...)")
  }

  ## Unfortunately the more natural lapply(badx, ...) does not work,
  ## because badx looses its class for length() and/or [[
  lapply(
    seq_along(badx),
    function(i) {
      x <- badx[[i]]
      f1 <- get_fun(x, 1)
      f2 <- get_fun(x, 3)
      line1 <- xml_attr(x, "line1")
      col1 <- xml_attr(x, "col1")
      col2 <- xml_attr(x, "col1")
      Lint(
        filename = source_file$filename,
        line_number = as.integer(line1),
        column_number = as.integer(col1),
        type = "warning",
        message = paste0(
          "Avoid ", f1, ":", f2, " expressions, use seq_len."),
        line = source_file$lines[line1],
        ranges = list(c(as.integer(col1), as.integer(col2)))
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
