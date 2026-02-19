#' @include lists.R

CHECKS$on_exit_add <- make_check(

  description = "on.exit() calls use add = TRUE",
  tags = c("best practice", "warning"),
  preps = "on_exit",

  gp = paste(
    "always use add = TRUE in on.exit() calls.",
    "Without it, each call to on.exit() overwrites the previous",
    "exit handler, which can lead to subtle bugs."
  ),

  check = function(state) {
    if (inherits(state$on_exit, "try-error")) return(NA)

    dat <- state$on_exit$calls
    bad <- dat[!dat$is_empty & !dat$has_add, ]

    if (nrow(bad) == 0) {
      list(status = TRUE, positions = list())
    } else {
      problems <- lapply(seq_len(nrow(bad)), function(i) {
        list(
          filename = file.path("R", bad$file[i]),
          line_number = bad$line[i],
          column_number = NA_integer_,
          ranges = list(),
          line = paste0(bad$name[i], ": on.exit() without add = TRUE")
        )
      })
      list(status = FALSE, positions = problems)
    }
  }
)

CHECKS$on_exit_missing <- make_check(

  description = "State-changing functions use on.exit() to restore state",
  tags = c("best practice", "warning"),
  preps = "on_exit",

  gp = paste(
    "use on.exit() to restore state after calling functions like",
    "options(), par(), setwd(), sink(), Sys.setenv(), or Sys.setlocale().",
    "Without on.exit(), the global state remains changed if the",
    "function exits early due to an error."
  ),

  check = function(state) {
    if (inherits(state$on_exit, "try-error")) return(NA)

    dat <- state$on_exit$state_changers
    missing <- dat[!dat$has_on_exit, ]

    if (nrow(missing) == 0) {
      list(status = TRUE, positions = list())
    } else {
      problems <- lapply(seq_len(nrow(missing)), function(i) {
        list(
          filename = file.path("R", missing$file[i]),
          line_number = missing$line[i],
          column_number = NA_integer_,
          ranges = list(),
          line = paste0(missing$name[i], " uses ", missing$changers[i])
        )
      })
      list(status = FALSE, positions = problems)
    }
  }
)
