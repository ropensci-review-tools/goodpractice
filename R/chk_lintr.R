
#' @include lists.R

CHECKS$assignment_linter <- make_check(

  description = "'<-' and not '=' is used for assignment",
  tags = "lintr",
  preps = "lintr",

  gp = "to use '<-' for assignment instead of '='.
        '<-' is the standard, and R users and developers are
        used it and it is easier to read your code for them if
        you use '<-'.",

  check = function(state) {
    linters <- vapply(state$lintr, "[[", "", "linter")
    ! "assignment_linter" %in% linters
  }
)

CHECKS$line_length_linter <- make_check(

  description = "Code lines are short",
  tags = "lintr",
  preps = "lintr",

  gp = "to avoid long code lines, it is bad for
        readability. Also, many people prefer editor windows
        that are about 80 characters wide. Try make your lines
        shorter than 80 characters",

  check = function(state) {
    linters <- vapply(state$lintr, "[[", "", "linter")
    ! "line_length_linter" %in% linters
  }
)
