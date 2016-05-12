
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
