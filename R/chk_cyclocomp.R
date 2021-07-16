
#' @include lists.R



CHECKS$cyclocomp <- make_check(

  description = "Functions are simple",
  tags = c("info", "code complexity"),
  preps = "cyclocomp",

  gp = function(state) {
    cyclocomp_limit <- getOption("goodpractice.cyclocomp.limit", 50)
    cyc <- state$cyclocomp
    long <- which(cyc$cyclocomp > cyclocomp_limit)
    funcs <- paste0(
      cyc$name[long],
      " (", cyc$cyclocomp[long], ")",
      collapse = ", "
    )
    paste0(
      "write short and simple functions. These functions have high
       cyclomatic complexity (>", cyclocomp_limit,"): ", funcs, ". ",
      "You can make them easier to reason about by encapsulating distinct steps
      of your function into subfunctions."
    )
  },

  check = function(state) {
    cyclocomp_limit <- getOption("goodpractice.cyclocomp.limit", 50)
    if (inherits(state$cyclocomp, "try-error")) return(NA)
    all(state$cyclocomp$cyclocomp <= cyclocomp_limit)
  }
)
