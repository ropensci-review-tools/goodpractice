#' @include lists.R customization.R treesitter.R

AVOIDED_PACKAGES <- list(
  multicore = "Use the 'parallel' package instead.",
  RCurl     = "Use 'httr2', 'curl', or 'crul' instead.",
  rjson     = "Use 'jsonlite' instead.",
  RJSONIO   = "Use 'jsonlite' instead.",
  XML       = "Use 'xml2' instead.",
  sp        = "Use 'sf' instead. 'sp' is deprecated.",
  rgdal     = "Use 'sf' or 'terra' instead. 'rgdal' was retired in 2023.",
  rgeos     = "Use 'sf' instead. 'rgeos' was retired in 2023.",
  maptools  = "Use 'sf' instead. 'maptools' was retired in 2023."
)

find_avoided_package_usage <- function(ts, pkg_name) {
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

  matches <- lapply(seq_along(ts$trees), function(i) {
    entry <- ts$trees[[i]]
    if (is.null(entry)) return(NULL)
    f <- names(ts$trees)[i]

    nodes <- c(
      treesitter::query_captures(library_q, entry$root)$node,
      treesitter::query_captures(ns_q, entry$root)$node
    )
    if (length(nodes) == 0) return(NULL)

    lapply(nodes, function(node) {
      list(
        filename = file.path("R", basename(f)),
        line_number = treesitter::node_start_point(node)$row + 1L,
        column_number = treesitter::node_start_point(node)$column + 1L,
        ranges = list(),
        line = trimws(treesitter::node_text(
          treesitter::node_parent(treesitter::node_parent(node))
        ))
      )
    })
  })

  unlist(matches, recursive = FALSE)
}

make_avoided_package_check <- function(pkg_name, reason) {
  force(pkg_name)
  force(reason)
  make_check(
    description = paste0("Avoid importing the '", pkg_name, "' package"),
    tags = c("warning", "best practice"),
    preps = character(),
    gp = paste0("avoid using the '", pkg_name, "' package. ", reason),
    check = function(state) {
      ts <- ts_get(state)
      if (length(ts$trees) == 0) {
        return(list(status = TRUE, positions = list()))
      }
      problems <- find_avoided_package_usage(ts, pkg_name)
      list(
        status = length(problems) == 0,
        positions = problems
      )
    }
  )
}

for (pkg in names(AVOIDED_PACKAGES)) {
  check_name <- paste0("no_import_", tolower(pkg))
  CHECKS[[check_name]] <- make_avoided_package_check(pkg, AVOIDED_PACKAGES[[pkg]])
}
