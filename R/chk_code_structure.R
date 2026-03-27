#' @include lists.R customization.R treesitter.R

## -- print methods return invisible -----------------------------------------

CHECKS$print_return_invisible <- make_check(

  description = "Print methods return the object invisibly",
  tags = c("warning", "best practice"),
  preps = "code_structure",

  gp = paste(
    "print methods should return the input object invisibly,",
    "e.g. {.code invisible(x)}. This allows chaining and consistent behaviour",
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

#' @noRd
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
  preps = "code_structure",

  gp = paste(
    "always use {.code add = TRUE} in {.fn on.exit} calls.",
    "Without it, each {.fn on.exit} overwrites previous exit handlers,",
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

## -- function length --------------------------------------------------------

#' @noRd
ts_function_length <- function(fn_node) {
  body <- treesitter::node_child_by_field_name(fn_node, "body")
  if (is.null(body)) return(0L)
  start <- treesitter::node_start_point(body)$row
  end <- treesitter::node_end_point(body)$row
  as.integer(end - start + 1L)
}

CHECKS$complexity_function_length <- make_check(

  description = "Functions are not too long",
  tags = c("warning", "best practice"),
  preps = "code_structure",

  gp = paste(
    "keep functions short and focused. Long functions are harder to",
    "understand, test, and maintain. Consider splitting into smaller helpers."
  ),

  check = function(state) {
    ts <- ts_get(state)
    if (length(ts$functions) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    limit <- getOption("goodpractice.function_length_limit", 50L)

    problems <- lapply(ts$functions, function(fn) {
      len <- ts_function_length(fn$fn_node)
      if (len <= limit) return(NULL)
      list(
        filename = file.path("R", basename(fn$file)),
        line_number = fn$line,
        column_number = NA_integer_,
        ranges = list(),
        line = paste0(fn$name, " (", len, " lines)")
      )
    })
    problems <- Filter(Negate(is.null), problems)

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

## -- unused internal functions ----------------------------------------------

<<<<<<< feature/dogfood
#' @noRd
ts_all_called_functions <- function(ts) {
=======
ts_all_referenced_functions <- function(ts) {
>>>>>>> main
  if (length(ts$trees) == 0) return(character())

  call_q <- treesitter::query(ts$language,
    "(call function: (identifier) @fn)"
  )

  refs <- unlist(lapply(ts$trees, function(entry) {
    if (is.null(entry)) return(NULL)
    root <- entry$root
    caps <- treesitter::query_captures(call_q, root)
    called <- vapply(
      caps$node[caps$name == "fn"],
      treesitter::node_text, character(1)
    )
    body_refs <- ts_body_identifiers(ts$language, entry)
    toplevel_refs <- ts_rhs_identifiers(ts$language, entry)
    c(called, body_refs, toplevel_refs)
  }))

  unique(refs)
}

ts_rhs_identifiers <- function(language, entry) {
  assign_q <- treesitter::query(language,
    "(binary_operator rhs: (identifier) @rhs)"
  )
  caps <- treesitter::query_captures(assign_q, entry$root)
  if (length(caps$name) == 0) return(character())
  vapply(
    caps$node[caps$name == "rhs"],
    treesitter::node_text, character(1)
  )
}

ts_body_identifiers <- function(language, entry) {
  id_q <- treesitter::query(language, "(identifier) @id")
  fns <- ts_file_functions(entry$root, "")
  unlist(lapply(fns, function(fn) {
    body <- treesitter::node_child_by_field_name(
      fn$fn_node, "body"
    )
    if (is.null(body)) return(NULL)
    caps <- treesitter::query_captures(id_q, body)
    vapply(
      caps$node[caps$name == "id"],
      treesitter::node_text, character(1)
    )
  }))
}

CHECKS$complexity_unused_internal <- make_check(

  description = "All internal functions are used",
  tags = c("warning", "best practice"),
  preps = "namespace",

  gp = paste(
    "remove or use internal functions that are defined but never called.",
    "Dead code increases maintenance burden."
  ),

  check = function(state) {
    ts <- ts_get(state)
    if (length(ts$functions) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    if (inherits(state$namespace, "try-error")) {
      return(list(status = NA, positions = list()))
    }

    ns <- state$namespace
    exports <- ns$exports
    s3m <- ns$S3methods
    s3methods <- if (nrow(s3m) > 0) {
      paste0(s3m[, 1], ".", s3m[, 2])
    } else {
      character()
    }
    exported <- c(exports, s3methods)

    all_defined <- vapply(ts$functions, `[[`, "", "name")
    internal <- setdiff(all_defined, exported)
    if (length(internal) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    called <- ts_all_referenced_functions(ts)
    unused <- setdiff(internal, called)
    if (length(unused) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    fn_lookup <- ts$functions
    names(fn_lookup) <- all_defined

    problems <- lapply(unused, function(name) {
      fn <- fn_lookup[[name]]
      list(
        filename = file.path("R", basename(fn$file)),
        line_number = fn$line,
        column_number = NA_integer_,
        ranges = list(),
        line = name
      )
    })

    list(
      status = FALSE,
      positions = problems
    )
  }
)

## -- duplicate function bodies ------------------------------------------------

#' @noRd
cross_file_duplicates <- function(df, key_col, file_col) {
  duped_keys <- unique(df[[key_col]][duplicated(df[[key_col]])])
  if (length(duped_keys) == 0) return(df[0, , drop = FALSE])

  candidates <- df[df[[key_col]] %in% duped_keys, , drop = FALSE]
  multi_file <- vapply(duped_keys, function(k) {
    matched <- candidates[[file_col]][candidates[[key_col]] == k]
    length(unique(basename(matched))) >= 2
  }, logical(1))

  candidates[candidates[[key_col]] %in% duped_keys[multi_file], , drop = FALSE]
}

#' @noRd
normalize_body_text <- function(fn_node) {
  body <- treesitter::node_child_by_field_name(fn_node, "body")
  if (is.null(body)) return("")
  gsub("\\s+", " ", trimws(treesitter::node_text(body)))
}

CHECKS$duplicate_function_bodies <- make_check(

  description = "No functions with identical bodies",
  tags = c("warning", "best practice"),
  preps = "code_structure",

  gp = paste(
    "consolidate functions with identical bodies into a single",
    "shared helper to reduce code duplication."
  ),

  check = function(state) {
    ts <- ts_get(state)
    if (length(ts$functions) < 2) {
      return(list(status = TRUE, positions = list()))
    }

    bodies <- vapply(ts$functions, function(fn) {
      normalize_body_text(fn$fn_node)
    }, character(1))

    min_dup_chars <- 20L
    trivial <- nchar(bodies) < min_dup_chars
    bodies[trivial] <- paste0("__trivial__", seq_along(bodies)[trivial])

    fn_df <- data.frame(
      body = bodies,
      name = vapply(ts$functions, `[[`, "", "name"),
      file = vapply(ts$functions, `[[`, "", "file"),
      line = vapply(ts$functions, function(fn) fn$line, numeric(1)),
      stringsAsFactors = FALSE
    )

    dupes <- cross_file_duplicates(fn_df, "body", "file")
    if (nrow(dupes) == 0) return(list(status = TRUE, positions = list()))

    problems <- lapply(seq_len(nrow(dupes)), function(i) {
      list(
        filename = file.path("R", basename(dupes$file[i])),
        line_number = dupes$line[i],
        column_number = NA_integer_,
        ranges = list(),
        line = dupes$name[i]
      )
    })

    list(status = FALSE, positions = problems)
  }
)
