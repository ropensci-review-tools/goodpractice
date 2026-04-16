#' @include lists.R

rd_find_topic <- function(rd_data, alias) {
  for (topic in rd_data) {
    if (alias %in% topic$aliases) return(topic)
  }
  NULL
}

rd_exported_aliases <- function(state) {
  ns <- state$namespace
  if (inherits(ns, "try-error")) return(character())

  setdiff(ns$exports, ns_s3_method_names(ns))
}

rd_check_field <- function(state, field, skip_internal = FALSE) {
  if (inherits(state$rd, "try-error")) return(na_result())

  rd_data <- state$rd
  if (length(rd_data) == 0) return(na_result())

  exports <- rd_exported_aliases(state)
  problems <- list()

  for (alias in exports) {
    topic <- rd_find_topic(rd_data, alias)
    if (is.null(topic)) next
    if (isTRUE(topic$is_reexport)) next
    if (skip_internal && isTRUE(topic$has_keyword_internal)) next

    if (!isTRUE(topic[[field]])) {
      problems[[length(problems) + 1]] <- check_position(
        file.path("man", topic$file),
        line = alias
      )
    }
  }

  check_result(length(problems) == 0, problems)
}

make_rd_check <- function(description, gp, field, tags = NULL,
                          skip_internal = FALSE) {
  make_check(
    description = description,
    tags = c("documentation", tags),
    preps = c("rd", "namespace"),
    gp = gp,
    check = function(state) rd_check_field(state, field, skip_internal)
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
    "functions using {.code \\\\value}."
  ),
  field = "has_value",
  skip_internal = TRUE
)

