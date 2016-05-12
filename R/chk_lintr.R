
get_lintr_position <- function(linter) {
  linter[c("filename", "line_number", "column_number", "ranges", "line")]
}

get_lintr_state <- function(state, linter) {
  linters <- vapply(state$lintr, "[[", "", "linter")
  list(
    status = ! linter %in% linters,
    positions = lapply(
      state$lintr[linters == linter],
      get_lintr_position
    )
  )
}

#' @include lists.R

CHECKS$lintr_assignment_linter <- make_check(

  description = "'<-' and not '=' is used for assignment",
  tags = "lintr",
  preps = "lintr",

  gp = "to use '<-' for assignment instead of '='.
        '<-' is the standard, and R users and developers are
        used it and it is easier to read your code for them if
        you use '<-'.",

  check = function(state) {
    get_lintr_state(state, "assignment_linter")
  }
)

CHECKS$lintr_line_length_linter <- make_check(

  description = "Code lines are short",
  tags = "lintr",
  preps = "lintr",

  gp = "to avoid long code lines, it is bad for
        readability. Also, many people prefer editor windows
        that are about 80 characters wide. Try make your lines
        shorter than 80 characters",

  check = function(state) {
    get_lintr_state(state, "line_length_linter")
  }
)

CHECKS$lintr_trailing_semicolon_linter <- make_check(

  description = "No trailing semicolons",
  tags = "lintr",
  preps = "lintr",

  gp = "to omit trailing semicolons from code lines.
        They are not needed and most R coding standards
        forbid them",

  check = function(state) {
    get_lintr_state(state, "trailing_semicolon_linter")
  }
)

CHECKS$lintr_attach_detach_linter = make_check(

  description = "Avoid attach and detach",
  tags = "lintr",
  preps = "lintr",

  gp = "to avoid the attach() and detach() functions,
        they are fragile and code that uses them will
        probably break sooner than later.",

  check = function(state) {
    get_lintr_state(state, "attach_detach_linter")
  }
)

CHECKS$lintr_setwd_linter = make_check(

  description = "Avoid setwd in R packages",
  tags = "lintr",
  preps = "lintr",

  gp = "to avoid calling setwd(), it changes the global environment.
        If you need it, consider using on.exit() to restore the
        working directory.",

  check = function(state) {
    get_lintr_state(state, "setwd_linter")
  }
)

CHECKS$lintr_sapply_linter = make_check(

  description = "Avoid sapply",
  tags = "lintr",
  preps = "lintr",

  gp = "to avoid sapply(), it is not type safe.
        It might return a vector, or a list, depending on the
        input data. Consider using vapply() instead.",

  check = function(state) {
    get_lintr_state(state, "sapply_linter")
  }
)

CHECKS$lintr_library_require_linter = make_check(

  description = "Avoid library and require in packages",
  tags = "lintr",
  preps = "lintr",

  gp = "to avoid the library() and require() functions,
        they change the global search path.
        If you need to use other packages, import them.
        If you need to load them explicitly, then consider
        loadNamespace() instead, or as a last resort, declare
        them as 'Depends' dependencies.",

  check = function(state) {
    get_lintr_state(state, "library_require_linter")
  }
)
