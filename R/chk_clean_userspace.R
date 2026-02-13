#' @include lists.R

CHECKS$clean_userspace <- make_check(

  description = "Examples and tests do not leave files behind",
  tags = c("CRAN"),
  preps = "clean_userspace",

  gp = "Examples and tests should not create files in the package directory.",

  check = function(state) {
    if (inherits(state$clean_userspace, "try-error")) return(NA)

    leftovers <- state$clean_userspace
    if (nrow(leftovers) == 0) {
      list(status = TRUE, positions = list())
    } else {
      problems <- lapply(seq_len(nrow(leftovers)), function(i) {
        list(
          filename = leftovers$file[i],
          line_number = NA_integer_,
          column_number = NA_integer_,
          ranges = list(),
          line = paste0("leftover from ", leftovers$source[i])
        )
      })
      list(status = FALSE, positions = problems)
    }
  }
)
