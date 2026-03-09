#' @include treesitter.R

CHECKS$print_return_invisible <- make_check(

  description = "Print methods return the object invisibly",
  tags = c("warning", "best practice"),
  preps = character(),

  gp = paste(
    "print methods should return the input object invisibly,",
    "e.g. invisible(x). This allows chaining and consistent behaviour",
    "with base R print methods."
  ),

  check = function(state) {
    ts <- ts_get(state)
    if (length(ts$functions) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    invisible_q <- treesitter::query(ts$language,
      "(call function: (identifier) @fn (#eq? @fn \"invisible\"))"
    )

    problems <- list()
    for (fn in ts$functions) {
      if (!startsWith(fn$name, "print.")) next

      body <- treesitter::node_child_by_field_name(fn$fn_node, "body")
      caps <- treesitter::query_captures(invisible_q, body)
      if (length(caps$name) > 0) next

      problems[[length(problems) + 1]] <- list(
        filename = file.path("R", basename(fn$file)),
        line_number = fn$line,
        column_number = NA_integer_,
        ranges = list(),
        line = fn$name
      )
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)
