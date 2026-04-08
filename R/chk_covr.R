
#' @include lists.R customization.R

CHECKS$covr <- make_check(

  description = "All code is unit tested",
  tags = c("warning", "code coverage", "covr"),
  preps = "covr",

  gp = function(state) {
    percent <- state$covr$pct_by_line
    if (is.nan(percent)) {
      return("write unit tests. No testable code was found in this package.")
    }
    paste0(
      "write unit tests for all functions, and all package code in ",
      "general. ", trunc(percent), "% of code lines are covered by ",
      "test cases."
    )
  },

  check = function(state) {
    if(inherits(state$covr, "try-error"))
      return(na_result())

    if (is.nan(state$covr$pct_by_line))
      return(check_result(TRUE))

    zero <- state$covr$zero
    if (NROW(zero) == 0) return(check_result(TRUE))
    
    positions <- lapply(seq_len(NROW(zero)), function(i) {
      check_position(zero$filename[i], zero$line[i], line = NA_character_)
    })
    
    check_result(FALSE, positions)
  }
)
