#' @include lists.R

#' @noRd
spelling_positions <- function(word, locations) {
  unlist(lapply(locations, function(loc) {
    parts <- strsplit(loc, ":")[[1]]
    line_nums <- as.integer(strsplit(parts[2], ",")[[1]])
    lapply(line_nums, function(ln) {
      list(
        filename = parts[1],
        line_number = ln,
        column_number = NA_integer_,
        ranges = list(),
        line = word
      )
    })
  }), recursive = FALSE)
}

CHECKS$spelling <- make_check(

  description = "No misspelled words in documentation",
  tags = c("info", "documentation"),
  preps = "spelling",

  gp = paste(
    "fix spelling errors in package documentation.",
    "Use {.code spelling::spell_check_package()} to review misspelled words",
    "and add false positives to {.file inst/WORDLIST}."
  ),

  check = function(state) {
    if (identical(state$spelling, "no_wordlist") ||
        inherits(state$spelling, "try-error")) {
      return(na_result())
    }

    res <- state$spelling
    if (nrow(res) == 0) {
      return(check_result(TRUE))
    }

    positions <- unlist(
      Map(spelling_positions, res$word, res$found),
      recursive = FALSE
    )
    check_result(FALSE, positions)
  }
)
