#' @include lists.R

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
  fns <- list()
  for (i in seq_len(n_children)) {
    child <- treesitter::node_child(root, i)
    if (treesitter::node_type(child) != "binary_operator") next
    lhs <- treesitter::node_child_by_field_name(child, "lhs")
    rhs <- treesitter::node_child_by_field_name(child, "rhs")
    if (is.null(rhs)) next
    if (treesitter::node_type(rhs) != "function_definition") next
    if (treesitter::node_type(lhs) != "identifier") next
    fns[[length(fns) + 1]] <- list(
      name = treesitter::node_text(lhs),
      file = file,
      line = treesitter::node_start_point(lhs)$row + 1L,
      fn_node = rhs
    )
  }
  fns
}

ts_parse <- function(path) {
  rdir <- file.path(path, "R")
  if (!dir.exists(rdir)) {
    return(list(trees = list(), functions = list()))
  }

  lang <- treesitter.r::language()
  p <- treesitter::parser(lang)
  rfiles <- list.files(rdir, pattern = "\\.[rR]$", full.names = TRUE)

  trees <- list()
  functions <- list()
  for (f in rfiles) {
    code <- tryCatch(
      paste(readLines(f, warn = FALSE), collapse = "\n"),
      error = function(e) NULL
    )
    if (is.null(code)) next

    tree <- treesitter::parser_parse(p, code)
    root <- treesitter::tree_root_node(tree)
    trees[[f]] <- list(tree = tree, root = root)
    functions <- c(functions, ts_file_functions(root, f))
  }

  list(trees = trees, functions = functions, language = lang)
}

ts_get <- function(state) {
  if (is.null(state$.cache$treesitter)) {
    state$.cache$treesitter <- ts_parse(state$path)
  }
  state$.cache$treesitter
}
