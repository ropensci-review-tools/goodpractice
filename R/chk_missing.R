#' @include lists.R

CHECKS$no_missing <- make_check(

  description = "Functions do not use missing() to check arguments",
  tags = c("warning", "best practice"),
  preps = character(),

  gp = paste(
    "avoid using missing() to check whether arguments were supplied.",
    "It makes functions difficult to call programmatically.",
    "Use a default value of NULL and check with is.null() instead."
  ),

  check = function(state) {
    path <- state$path
    rdir <- file.path(path, "R")

    if (!dir.exists(rdir)) {
      return(list(status = TRUE, positions = list()))
    }

    rfiles <- list.files(rdir, pattern = "\\.[rR]$", full.names = TRUE)
    problems <- list()

    for (f in rfiles) {
      lines <- readLines(f, warn = FALSE)
      for (i in seq_along(lines)) {
        line <- lines[i]
        if (grepl("^\\s*#", line)) next
        if (grepl("\\bmissing\\s*\\(", line)) {
          problems[[length(problems) + 1]] <- list(
            filename = file.path("R", basename(f)),
            line_number = i,
            column_number = NA_integer_,
            ranges = list(),
            line = trimws(line)
          )
        }
      }
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)
