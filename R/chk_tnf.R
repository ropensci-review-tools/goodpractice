
#' @include lists.R
#' @importFrom tools checkTnF

CHECKS$truefalse_not_tf <- make_check(

  description = "TRUE and FALSE is used, not T and F",
  tags = "warning",
  preps = character(),

  gp = "avoid 'T' and 'F', as they are just variables which are set to the
        logicals 'TRUE' and 'FALSE' by default, but are not reserved words
        and hence can be overwritten by the user.  Hence, one should
        always use 'TRUE' and 'FALSE' for the logicals.",

  check = function(state) {
    tf <- tryCatch(
      checkTnF(dir = state$path),
      error = function(e) list()
    )

    if (length(tf) == 0) return(list(status = TRUE, positions = list()))

    absfilenames <- rep(names(tf), vapply(tf, length, 1L))
    filenames <- file.path(
      basename(dirname(absfilenames)),
      basename(absfilenames)
    )

    positions <- lapply(filenames, function(x) {
      list(
        filename = x,
        line_number = NA_integer_,
        column_number = NA_integer_,
        ranges = list(),
        line = NA_character_
      )
    })

    list(
      status = FALSE,
      positions = positions
    )
  }
)
