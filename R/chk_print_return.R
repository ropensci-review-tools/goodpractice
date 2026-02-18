#' @include lists.R

CHECKS$print_return_invisible <- make_check(

  description = "Print methods return the object invisibly",
  tags = c("warning", "best practice"),
  preps = character(),

  gp = paste(
    "print methods should return the input object invisibly,",
    "e.g. invisible(x). This allows chaining and consistent behaviour",
    "with base R print methods."
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
      n <- length(lines)
      i <- 1L

      while (i <= n) {
        line <- lines[i]
        if (grepl("^\\s*print\\.\\w+\\s*(<-|=)\\s*function\\s*\\(", line)) {
          def_start <- i
          brace_count <- 0L
          body_start <- i
          found_open <- FALSE

          for (j in i:n) {
            chars <- strsplit(lines[j], "")[[1]]
            for (ch in chars) {
              if (ch == "{") {
                if (!found_open) body_start <- j
                found_open <- TRUE
                brace_count <- brace_count + 1L
              } else if (ch == "}") {
                brace_count <- brace_count - 1L
              }
            }
            if (found_open && brace_count == 0L) {
              body_lines <- lines[body_start:j]
              has_invisible <- any(grepl("\\binvisible\\s*\\(", body_lines))
              if (!has_invisible) {
                problems[[length(problems) + 1]] <- list(
                  filename = file.path("R", basename(f)),
                  line_number = def_start,
                  column_number = NA_integer_,
                  ranges = list(),
                  line = trimws(lines[def_start])
                )
              }
              i <- j
              break
            }
          }
        }
        i <- i + 1L
      }
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)
