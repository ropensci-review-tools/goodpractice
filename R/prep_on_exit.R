#' @include lists.R

find_calls_shallow <- function(expr, targets) {
  found <- character()
  recurse <- function(e) {
    if (is.call(e)) {
      fn <- deparse(e[[1]])
      if (fn %in% targets) found <<- c(found, fn)
      if (fn == "function") return()
      for (i in seq_along(e)) recurse(e[[i]])
    } else if (is.recursive(e)) {
      for (i in seq_along(e)) recurse(e[[i]])
    }
  }
  recurse(expr)
  found
}

find_on_exit_calls <- function(expr) {
  calls <- list()
  recurse <- function(e) {
    if (is.call(e)) {
      fn <- deparse(e[[1]])
      if (fn == "function") return()
      if (fn == "on.exit") {
        args <- as.list(e)[-1]
        is_empty <- length(args) == 0 ||
          (length(args) == 1 && is.symbol(args[[1]]) && nchar(args[[1]]) == 0)
        has_add <- "add" %in% names(args)
        calls[[length(calls) + 1]] <<- list(
          is_empty = is_empty,
          has_add = has_add
        )
      }
      for (i in seq_along(e)) recurse(e[[i]])
    } else if (is.recursive(e)) {
      for (i in seq_along(e)) recurse(e[[i]])
    }
  }
  recurse(expr)
  calls
}

STATE_CHANGING_CALLS <- c(
  "options", "par", "setwd", "sink",
  "Sys.setenv", "Sys.setlocale", "Sys.unsetenv"
)

PREPS$on_exit <- function(state, path = state$path, quiet) {
  rfiles <- r_package_files(path)
  state_changers <- data.frame(
    file = character(),
    name = character(),
    line = integer(),
    changers = character(),
    has_on_exit = logical(),
    stringsAsFactors = FALSE
  )
  on_exit_calls <- data.frame(
    file = character(),
    name = character(),
    line = integer(),
    is_empty = logical(),
    has_add = logical(),
    stringsAsFactors = FALSE
  )

  for (f in rfiles) {
    if (!file.exists(f)) next
    exprs <- tryCatch(
      parse(f, keep.source = TRUE),
      error = function(e) NULL
    )
    if (is.null(exprs) || length(exprs) == 0) next

    srcrefs <- attr(exprs, "srcref")

    for (i in seq_along(exprs)) {
      e <- exprs[[i]]
      if (!is.call(e)) next

      op <- deparse(e[[1]])
      if (!(op %in% c("<-", "=")) || length(e) != 3) next
      if (!is.call(e[[3]])) next
      if (!identical(deparse(e[[3]][[1]]), "function")) next

      name <- deparse(e[[2]])
      body_expr <- e[[3]][[3]]
      line <- if (!is.null(srcrefs[[i]])) srcrefs[[i]][1] else NA_integer_

      changers <- find_calls_shallow(body_expr, STATE_CHANGING_CALLS)
      oe_calls <- find_on_exit_calls(body_expr)
      has_on_exit <- length(oe_calls) > 0

      if (length(changers) > 0) {
        state_changers <- rbind(state_changers, data.frame(
          file = basename(f),
          name = name,
          line = line,
          changers = paste(unique(changers), collapse = ", "),
          has_on_exit = has_on_exit,
          stringsAsFactors = FALSE
        ))
      }

      for (oe in oe_calls) {
        on_exit_calls <- rbind(on_exit_calls, data.frame(
          file = basename(f),
          name = name,
          line = line,
          is_empty = oe$is_empty,
          has_add = oe$has_add,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  state$on_exit <- list(
    state_changers = state_changers,
    calls = on_exit_calls
  )
  state
}
