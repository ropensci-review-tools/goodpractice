#' @include lists.R customization.R treesitter.R

AVOIDED_PACKAGES <- list(
  multicore = "Use the 'parallel' package instead."
)

find_avoided_package_usage <- function(ts, pkg_name) {
  problems <- list()

  library_q <- treesitter::query(ts$language, paste0(
    "(call function: (identifier) @fn ",
    "(#match? @fn \"^(library|require)$\") ",
    "arguments: (arguments (argument value: (identifier) @pkg ",
    "(#eq? @pkg \"", pkg_name, "\"))))"
  ))

  ns_q <- treesitter::query(ts$language, paste0(
    "(namespace_operator lhs: (identifier) @ns ",
    "(#eq? @ns \"", pkg_name, "\"))"
  ))

  for (i in seq_along(ts$trees)) {
    entry <- ts$trees[[i]]
    if (is.null(entry)) next
    f <- names(ts$trees)[i]

    for (q in list(library_q, ns_q)) {
      caps <- treesitter::query_captures(q, entry$root)
      for (j in seq_along(caps$name)) {
        node <- caps$node[[j]]
        problems[[length(problems) + 1]] <- list(
          filename = file.path("R", basename(f)),
          line_number = treesitter::node_start_point(node)$row + 1L,
          column_number = treesitter::node_start_point(node)$column + 1L,
          ranges = list(),
          line = trimws(treesitter::node_text(
            treesitter::node_parent(treesitter::node_parent(node))
          ))
        )
      }
    }
  }

  problems
}

CHECKS$no_import_multicore <- make_check(

  description = "Avoid importing the 'multicore' package",
  tags = c("warning", "best practice"),
  preps = character(),

  gp = paste(
    "avoid using the 'multicore' package.",
    AVOIDED_PACKAGES[["multicore"]]
  ),

  check = function(state) {
    ts <- ts_get(state)
    if (length(ts$trees) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    problems <- find_avoided_package_usage(ts, "multicore")

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)
