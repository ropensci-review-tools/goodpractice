#' @include lists.R

#' @noRd
ts_nodes_equal <- function(a, b) {
  identical(treesitter::node_start_point(a),
            treesitter::node_start_point(b)) &&
    identical(treesitter::node_end_point(a),
              treesitter::node_end_point(b))
}

ts_inside_nested_function <- function(node, fn_body) {
  parent <- treesitter::node_parent(node)
  while (!is.null(parent) && !ts_nodes_equal(parent, fn_body)) {
    if (treesitter::node_type(parent) == "function_definition") return(TRUE)
    parent <- treesitter::node_parent(parent)
  }
  FALSE
}

ts_body_has_call <- function(fn_node, call_query) {
  body <- treesitter::node_child_by_field_name(fn_node, "body")
  caps <- treesitter::query_captures(call_query, body)
  for (j in which(caps$name == "fn")) {
    if (!ts_inside_nested_function(caps$node[[j]], body)) return(TRUE)
  }
  FALSE
}

ts_file_functions <- function(root, file) {
  n_children <- treesitter::node_child_count(root)
  fns <- vector("list", n_children)
  k <- 0L
  for (i in seq_len(n_children)) {
    child <- treesitter::node_child(root, i)
    if (treesitter::node_type(child) != "binary_operator") next
    lhs <- treesitter::node_child_by_field_name(child, "lhs")
    rhs <- treesitter::node_child_by_field_name(child, "rhs")
    if (is.null(rhs)) next
    if (treesitter::node_type(rhs) != "function_definition") next
    if (treesitter::node_type(lhs) != "identifier") next
    k <- k + 1L
    fns[[k]] <- list(
      name = treesitter::node_text(lhs),
      file = file,
      line = treesitter::node_start_point(lhs)$row + 1L,
      fn_node = rhs
    )
  }
  fns[seq_len(k)]
}

ts_parse <- function(path, exclude_path = character()) {
  rdir <- file.path(path, "R")
  if (!dir.exists(rdir)) {
    return(list(trees = list(), functions = list()))
  }

  lang <- treesitter.r::language()
  p <- treesitter::parser(lang)
  rfiles <- list.files(rdir, pattern = default_r_file_pattern(), full.names = TRUE)
  rfiles <- filter_excluded_paths(rfiles, path, exclude_path)

  trees <- vector("list", length(rfiles))
  names(trees) <- rfiles
  fn_lists <- vector("list", length(rfiles))
  for (i in seq_along(rfiles)) {
    f <- rfiles[[i]]
    code <- tryCatch(
      paste(readLines(f, warn = FALSE), collapse = "\n"),
      error = function(e) NULL
    )
    if (is.null(code)) next

    tree <- treesitter::parser_parse(p, code)
    root <- treesitter::tree_root_node(tree)
    trees[[i]] <- list(tree = tree, root = root)
    fn_lists[[i]] <- ts_file_functions(root, f)
  }

  functions <- unlist(fn_lists, recursive = FALSE)
  list(trees = trees, functions = functions, language = lang)
}

S4_CALL_NAMES <- c(
  "setMethod", "setGeneric", "setClass",
  "setReplaceMethod", "setValidity", "setIs"
)

ts_s4_call_ranges <- function(ts) {
  if (length(ts$trees) == 0) return(list())

  s4_query <- treesitter::query(ts$language, paste0(
    "(call function: (identifier) @fn (#match? @fn \"^(",
    paste(S4_CALL_NAMES, collapse = "|"),
    ")$\"))"
  ))

  ranges <- unlist(lapply(names(ts$trees), function(file) {
    entry <- ts$trees[[file]]
    if (is.null(entry)) return(NULL)
    caps <- treesitter::query_captures(s4_query, entry$root)
    idxs <- which(caps$name == "fn")
    if (length(idxs) == 0) return(NULL)

    lapply(idxs, function(j) {
      call_node <- treesitter::node_parent(caps$node[[j]])
      list(
        file = basename(file),
        start = treesitter::node_start_point(call_node)$row + 1L,
        end = treesitter::node_end_point(call_node)$row + 1L
      )
    })
  }), recursive = FALSE)

  if (is.null(ranges)) list() else ranges
}

ts_get <- function(state) {
  if (is.null(state$.cache$treesitter)) {
    state$.cache$treesitter <- ts_parse(
      state$path, state$exclude_path %||% character()
    )
  }
  state$.cache$treesitter
}

filter_s4_assignment_false_positives <- function(state, result) {
  if (isTRUE(result$status) || is.na(result$status)) return(result)

  ts <- ts_get(state)
  s4_ranges <- ts_s4_call_ranges(ts)
  if (length(s4_ranges) == 0) return(result)

  result$positions <- Filter(function(pos) {
    f <- basename(pos$filename)
    ln <- pos$line_number
    !any(vapply(s4_ranges, function(r) {
      f == r$file && ln >= r$start && ln <= r$end
    }, logical(1)))
  }, result$positions)

  result$status <- length(result$positions) == 0
  result
}
