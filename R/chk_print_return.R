#' @include lists.R

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
    path <- state$path
    rdir <- file.path(path, "R")

    if (!dir.exists(rdir)) {
      return(list(status = TRUE, positions = list()))
    }

    lang <- treesitter.r::language()
    p <- treesitter::parser(lang)
    invisible_query <- treesitter::query(lang,
      "(call function: (identifier) @fn (#eq? @fn \"invisible\"))"
    )

    rfiles <- list.files(rdir, pattern = "\\.[rR]$", full.names = TRUE)
    problems <- list()

    for (f in rfiles) {
      code <- tryCatch(
        paste(readLines(f, warn = FALSE), collapse = "\n"),
        error = function(e) NULL
      )
      if (is.null(code)) next

      tree <- treesitter::parser_parse(p, code)
      root <- treesitter::tree_root_node(tree)

      n_children <- treesitter::node_child_count(root)
      for (i in seq_len(n_children)) {
        child <- treesitter::node_child(root, i)
        if (treesitter::node_type(child) != "binary_operator") next
        lhs <- treesitter::node_child_by_field_name(child, "lhs")
        rhs <- treesitter::node_child_by_field_name(child, "rhs")
        if (is.null(rhs)) next
        if (treesitter::node_type(rhs) != "function_definition") next
        if (treesitter::node_type(lhs) != "identifier") next

        name <- treesitter::node_text(lhs)
        if (!startsWith(name, "print.")) next

        body <- treesitter::node_child_by_field_name(rhs, "body")
        caps <- treesitter::query_captures(invisible_query, body)
        if (length(caps$name) > 0) next

        problems[[length(problems) + 1]] <- list(
          filename = file.path("R", basename(f)),
          line_number = treesitter::node_start_point(lhs)$row + 1L,
          column_number = NA_integer_,
          ranges = list(),
          line = name
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
