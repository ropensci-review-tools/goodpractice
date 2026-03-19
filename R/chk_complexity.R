#' @include lists.R customization.R treesitter.R

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
  preps = character(),

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

## --------------------------------------------------------------------

ts_all_called_functions <- function(ts) {
  if (length(ts$trees) == 0) return(character())

  call_q <- treesitter::query(ts$language,
    "(call function: (identifier) @fn)"
  )

  calls <- unlist(lapply(ts$trees, function(entry) {
    if (is.null(entry)) return(NULL)
    caps <- treesitter::query_captures(call_q, entry$root)
    vapply(caps$node[caps$name == "fn"], treesitter::node_text, character(1))
  }))

  unique(calls)
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

    called <- ts_all_called_functions(ts)
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
