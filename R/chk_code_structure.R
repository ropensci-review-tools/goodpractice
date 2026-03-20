#' @include lists.R customization.R treesitter.R

## -- print methods return invisible -----------------------------------------

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

## -- on.exit() should have add = TRUE ---------------------------------------

on_exit_call_missing_add <- function(call_node) {
  args_node <- treesitter::node_child_by_field_name(call_node, "arguments")
  if (is.null(args_node)) return(FALSE)

  n_children <- treesitter::node_child_count(args_node)
  if (n_children <= 2L) return(FALSE)

  for (i in seq_len(n_children)) {
    child <- treesitter::node_child(args_node, i)
    if (treesitter::node_type(child) != "argument") next
    name_node <- treesitter::node_child_by_field_name(child, "name")
    if (!is.null(name_node) && treesitter::node_text(name_node) == "add") {
      return(FALSE)
    }
  }

  TRUE
}

CHECKS$on_exit_has_add <- make_check(

  description = "on.exit() calls include add = TRUE",
  tags = c("warning", "best practice"),
  preps = character(),

  gp = paste(
    "always use add = TRUE in on.exit() calls.",
    "Without it, each on.exit() overwrites previous exit handlers,",
    "which is a common source of bugs."
  ),

  check = function(state) {
    ts <- ts_get(state)
    if (length(ts$functions) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    on_exit_q <- treesitter::query(ts$language,
      "(call function: (identifier) @fn (#eq? @fn \"on.exit\"))"
    )

    problems <- list()
    for (fn in ts$functions) {
      if (!ts_body_has_call(fn$fn_node, on_exit_q)) next

      body <- treesitter::node_child_by_field_name(fn$fn_node, "body")
      caps <- treesitter::query_captures(on_exit_q, body)
      for (j in which(caps$name == "fn")) {
        if (ts_inside_nested_function(caps$node[[j]], body)) next
        call_node <- treesitter::node_parent(caps$node[[j]])
        if (on_exit_call_missing_add(call_node)) {
          problems[[length(problems) + 1]] <- list(
            filename = file.path("R", basename(fn$file)),
            line_number = treesitter::node_start_point(call_node)$row + 1L,
            column_number = NA_integer_,
            ranges = list(),
            line = fn$name
          )
        }
      }
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)
