#' @include lists.R

find_calls_shallow <- function(expr, targets) {
  env <- new.env(parent = emptyenv())
  env$found <- character()
  recurse <- function(e) {
    if (is.call(e)) {
      fn <- deparse(e[[1]])
      if (fn %in% targets) env$found <- c(env$found, fn)
      if (fn == "function") return()
      for (i in seq_along(e)) recurse(e[[i]])
    } else if (is.recursive(e)) {
      for (i in seq_along(e)) recurse(e[[i]])
    }
  }
  recurse(expr)
  env$found
}

find_on_exit_calls <- function(expr) {
  env <- new.env(parent = emptyenv())
  env$calls <- list()
  recurse <- function(e) {
    if (is.call(e)) {
      fn <- deparse(e[[1]])
      if (fn == "function") return()
      if (fn == "on.exit") {
        args <- as.list(e)[-1]
        is_empty <- length(args) == 0 ||
          (length(args) == 1 && is.symbol(args[[1]]) && nchar(args[[1]]) == 0)
        has_add <- "add" %in% names(args)
        env$calls[[length(env$calls) + 1]] <- list(
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
  env$calls
}

STATE_CHANGING_CALLS <- c(
  "options", "par", "setwd", "sink",
  "Sys.setenv", "Sys.setlocale", "Sys.unsetenv"
)

PREPS$on_exit <- function(state, path = state$path, quiet) {
  fns <- state$functions %||% parse_package_functions(path)

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

  for (fn in fns) {
    changers <- find_calls_shallow(fn$body, STATE_CHANGING_CALLS)
    oe_calls <- find_on_exit_calls(fn$body)
    has_on_exit <- length(oe_calls) > 0

    if (length(changers) > 0) {
      state_changers <- rbind(state_changers, data.frame(
        file = basename(fn$file),
        name = fn$name,
        line = fn$line,
        changers = paste(unique(changers), collapse = ", "),
        has_on_exit = has_on_exit,
        stringsAsFactors = FALSE
      ))
    }

    for (oe in oe_calls) {
      on_exit_calls <- rbind(on_exit_calls, data.frame(
        file = basename(fn$file),
        name = fn$name,
        line = fn$line,
        is_empty = oe$is_empty,
        has_add = oe$has_add,
        stringsAsFactors = FALSE
      ))
    }
  }

  state$on_exit <- list(
    state_changers = state_changers,
    calls = on_exit_calls
  )
  state
}
