
#' @include lists.R

cyclocomp_limit <- function() {
  getOption("goodpractice.cyclocomp_limit", 15)
}

CHECKS$cyclocomp <- make_check(

  description = "Functions are simple",
  tags = c("info", "code complexity"),
  preps = "cyclocomp",

  gp = function(state) {
    limit <- cyclocomp_limit()
    cyc <- state$cyclocomp
    long <- which(cyc$cyclocomp > limit)
    funcs <- paste0(
      cyc$name[long],
      " (", cyc$cyclocomp[long], ")",
      collapse = ", "
    )
    paste0(
      "write short and simple functions.",
      " These functions have high",
      " cyclomatic complexity (>", limit, "): ",
      funcs, ". ",
      "You can make them easier to reason about",
      " by encapsulating distinct steps",
      " of your function into subfunctions."
    )
  },

  check = function(state) {
    if (inherits(state$cyclocomp, "try-error")) return(NA)
    all(state$cyclocomp$cyclocomp <= cyclocomp_limit())
  }
)
