#' @include lists.R

has_invisible_call <- function(expr) {
  if (is.call(expr)) {
    fn <- deparse(expr[[1]])
    if (fn == "invisible") return(TRUE)
    for (i in seq_along(expr)) {
      if (has_invisible_call(expr[[i]])) return(TRUE)
    }
  }
  FALSE
}

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
    fns <- parse_package_functions(state$path)
    problems <- list()

    for (fn in fns) {
      if (!grepl("^print\\.", fn$name)) next

      if (!has_invisible_call(fn$body)) {
        problems[[length(problems) + 1]] <- list(
          filename = file.path("R", basename(fn$file)),
          line_number = fn$line,
          column_number = NA_integer_,
          ranges = list(),
          line = fn$name
        )
      }
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)
