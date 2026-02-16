#' @include lists.R

rd_na_result <- function() {
  list(status = NA, positions = list())
}

rd_find_topic <- function(rd_data, alias) {
  for (topic in rd_data) {
    if (alias %in% topic$aliases) return(topic)
  }
  NULL
}

rd_exported_aliases <- function(state) {
  ns <- state$namespace
  if (inherits(ns, "try-error")) return(character())

  exports <- ns$exports
  s3m <- ns$S3methods
  s3methods <- if (nrow(s3m) > 0) {
    paste0(s3m[, 1], ".", s3m[, 2])
  } else {
    character()
  }
  setdiff(exports, s3methods)
}

make_rd_check <- function(description, gp, field, tags = NULL) {
  make_check(
    description = description,
    tags = c("documentation", tags),
    preps = c("rd", "namespace"),
    gp = gp,

    check = function(state) {
      if (inherits(state$rd, "try-error")) return(rd_na_result())

      rd_data <- state$rd
      if (length(rd_data) == 0) return(rd_na_result())

      exports <- rd_exported_aliases(state)
      problems <- list()

      for (alias in exports) {
        topic <- rd_find_topic(rd_data, alias)
        if (is.null(topic)) next

        if (!isTRUE(topic[[field]])) {
          problems[[length(problems) + 1]] <- list(
            filename = file.path("man", topic$file),
            line_number = NA_integer_,
            column_number = NA_integer_,
            ranges = list(),
            line = alias
          )
        }
      }

      list(
        status = length(problems) == 0,
        positions = problems
      )
    }
  )
}

CHECKS$rd_has_examples <- make_rd_check(
  description = "Exported functions have \\examples in .Rd",
  gp = "Add examples to all exported functions.",
  field = "has_examples"
)

CHECKS$rd_has_return <- make_rd_check(
  description = "Exported functions have \\value in .Rd",
  gp = paste(
    "Document return values for exported (non-method)",
    "functions using \\value."
  ),
  field = "has_value"
)

CHECKS$rd_examples_dontrun <- make_check(

  description = "Examples do not use \\dontrun",
  tags = c("documentation", "CRAN"),
  preps = "rd",

  gp = paste(
    "Replace \\dontrun{} with \\donttest{} in examples.",
    "\\dontrun{} should only be used if the example truly cannot be",
    "executed by the user (e.g. missing API keys or external software)."
  ),

  check = function(state) {
    if (inherits(state$rd, "try-error")) return(rd_na_result())

    rd_data <- state$rd
    if (length(rd_data) == 0) return(rd_na_result())

    problems <- list()

    for (topic in rd_data) {
      if (!topic$has_examples) next
      if (topic$has_dontrun) {
        problems[[length(problems) + 1]] <- list(
          filename = file.path("man", topic$file),
          line_number = NA_integer_,
          column_number = NA_integer_,
          ranges = list(),
          line = paste(topic$aliases, collapse = ", ")
        )
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

CHECKS$rd_examples_runnable <- make_check(

  description = "Examples have runnable code outside \\dontrun/\\donttest",
  tags = c("documentation", "CRAN"),
  preps = "rd",

  gp = paste(
    "Include some directly runnable example code.",
    "Wrapping all example code in \\dontrun{} or \\donttest{} means",
    "nothing is checked by R CMD check. At least some examples should",
    "run unconditionally."
  ),

  check = function(state) {
    if (inherits(state$rd, "try-error")) return(rd_na_result())

    rd_data <- state$rd
    if (length(rd_data) == 0) return(rd_na_result())

    problems <- list()

    for (topic in rd_data) {
      if (!topic$has_examples) next
      if (!topic$has_runnable_code) {
        problems[[length(problems) + 1]] <- list(
          filename = file.path("man", topic$file),
          line_number = NA_integer_,
          column_number = NA_integer_,
          ranges = list(),
          line = paste(topic$aliases, collapse = ", ")
        )
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)
