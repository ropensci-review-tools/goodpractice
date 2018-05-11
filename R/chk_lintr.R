
get_lintr_position <- function(linter) {
  linter[c("filename", "line_number", "column_number", "ranges", "line")]
}

get_lintr_state <- function(state, linter) {
  if(inherits(state$lintr, "try-error")) {
    return(list(status = NA, position = list()))
  }
  
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
  tags = c("style", "lintr"),
  preps = "lintr",

  gp = "use '<-' for assignment instead of '='.
        '<-' is the standard, and R users and developers are
        used it and it is easier to read your code for them if
        you use '<-'.",

  check = function(state) {
    get_lintr_state(state, "assignment_linter")
  }
)

CHECKS$lintr_line_length_linter <- make_check(

  description = "Code lines are short",
  tags = c("style", "lintr"),
  preps = "lintr",

  gp = "avoid long code lines, it is bad for
        readability. Also, many people prefer editor windows
        that are about 80 characters wide. Try make your lines
        shorter than 80 characters",

  check = function(state) {
    get_lintr_state(state, "line_length_linter")
  }
)

CHECKS$lintr_trailing_semicolon_linter <- make_check(

  description = "No trailing semicolons",
  tags = c("style", "lintr"),
  preps = "lintr",

  gp = "omit trailing semicolons from code lines.
        They are not needed and most R coding standards
        forbid them",

  check = function(state) {
    get_lintr_state(state, "trailing_semicolon_linter")
  }
)

CHECKS$lintr_attach_detach_linter <- make_check(

  description = "Avoid attach and detach",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid the attach() and detach() functions,
        they are fragile and code that uses them will
        probably break sooner than later.",

  check = function(state) {
    get_lintr_state(state, "attach_detach_linter")
  }
)

CHECKS$lintr_setwd_linter <- make_check(

  description = "Avoid setwd in R packages",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid calling setwd(), it changes the global environment.
        If you need it, consider using on.exit() to restore the
        working directory.",

  check = function(state) {
    get_lintr_state(state, "setwd_linter")
  }
)

CHECKS$lintr_sapply_linter <- make_check(

  description = "Avoid sapply",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid sapply(), it is not type safe.
        It might return a vector, or a list, depending on the
        input data. Consider using vapply() instead.",

  check = function(state) {
    get_lintr_state(state, "sapply_linter")
  }
)

CHECKS$lintr_library_require_linter <- make_check(

  description = "Avoid library and require in packages",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid the library() and require() functions,
        they change the global search path.
        If you need to use other packages, import them.
        If you need to load them explicitly, then consider
        loadNamespace() instead, or as a last resort, declare
        them as 'Depends' dependencies.",

  check = function(state) {
    if(inherits(state$lintr, "try-error")) {
      return(list(status = NA, position = list()))
    }
    
    res <- get_lintr_state(state, "library_require_linter")

    ## library() and require() are OK in tests and vignettes
    res$positions <- Filter(
      f = function(x) grepl("^R[/(\\)]", x$filename),
      res$positions
    )
    res$status <- length(res$positions) == 0
    res
  }
)

CHECKS$lintr_seq_linter <- make_check(

  description = "Avoid 1:length(...) and similar expressions",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid 1:length(...), 1:nrow(...), 1:ncol(...),
        1:NROW(...) and 1:NCOL(...) expressions. They are error
        prone and result 1:0 if the expression on the right hand
        side is zero. Use seq_len() or seq_along() instead.",

  check = function(state) {
    get_lintr_state(state, "seq_linter")
  }
)
