
#' @include lists.R

CHECKS$covr <- make_check(

  description = "All code is unit tested",
  tags = c("warning", "code coverage", "covr"),
  preps = "covr",

  gp = "write unit tests for all functions, and all package code in
        general. At least some lines are not covered in this package.",

  check = function(state) {
    zero <- state$covr$zero
    if (NROW(zero) == 0) return(list(status = TRUE, positions = list()))

    positions <- lapply(seq_len(NROW(zero)), function(i) {
      list(
        filename = zero$filename[i],
        line_number = zero$line[i],
        column_number = NA_integer_,
        ranges = list(),
        line = NA_character_
      )
    })

    list(status = FALSE, positions = positions)
  }
)
