
#' @include treesitter.R

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

  gp = "use {.code <-} for assignment instead of {.code =}.
        {.code <-} is the standard, and R users and developers are
        used it and it is easier to read your code for them if
        you use {.code <-}.",

  check = function(state) {
    result <- get_lintr_state(state, "assignment_linter")
    filter_s4_assignment_false_positives(state, result)
  }
)

CHECKS$lintr_line_length_linter <- make_check(

  description = "Code lines are short",
  tags = c("style", "lintr"),
  preps = "lintr",

  gp = "avoid long code lines, it is bad for
        readability. Also, many people prefer editor windows
        that are about 80 characters wide. Try making your lines
        shorter than 80 characters",

  check = function(state) {
    get_lintr_state(state, "line_length_linter")
  }
)

CHECKS$lintr_semicolon_linter <- make_check(

  description = "No trailing semicolons",
  tags = c("style", "lintr"),
  preps = "lintr",

  gp = "omit trailing semicolons from code lines.
        They are not needed and most R coding standards
        forbid them",

  check = function(state) {
    get_lintr_state(state, "semicolon_linter")
  }
)

CHECKS$lintr_attach_detach_linter <- make_check(

  description = "Avoid attach and detach",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid the {.fn attach} and {.fn detach} functions,
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

  gp = "avoid calling {.fn setwd}, it changes the global environment.
        If you need it, consider using {.fn on.exit} to restore the
        working directory.",

  check = function(state) {
    get_lintr_state(state, "setwd_linter")
  }
)

CHECKS$lintr_sapply_linter <- make_check(

  description = "Avoid sapply",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid {.fn sapply}, it is not type safe.
        It might return a vector, or a list, depending on the
        input data. Consider using {.fn vapply} instead.",

  check = function(state) {
    get_lintr_state(state, "sapply_linter")
  }
)

CHECKS$lintr_library_require_linter <- make_check(

  description = "Avoid library and require in packages",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid the {.fn library} and {.fn require} functions,
        they change the global search path.
        If you need to use other packages, import them.
        If you need to load them explicitly, then consider
        {.fn loadNamespace} instead, or as a last resort, declare
        them as {.field Depends} dependencies.",

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

  gp = "avoid {.code 1:length(...)}, {.code 1:nrow(...)}, {.code 1:ncol(...)},
        {.code 1:NROW(...)} and {.code 1:NCOL(...)} expressions. They are error
        prone and result {.code 1:0} if the expression on the right hand
        side is zero. Use {.fn seq_len} or {.fn seq_along} instead.",

  check = function(state) {
    get_lintr_state(state, "seq_linter")
  }
)
