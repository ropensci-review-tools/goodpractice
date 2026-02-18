#' @include lists.R

CHECKS$spelling <- make_check(

  description = "No misspelled words in documentation",
  tags = c("info", "documentation"),
  preps = "spelling",

  gp = paste(
    "fix spelling errors in package documentation.",
    "Use spelling::spell_check_package() to review misspelled words",
    "and add false positives to inst/WORDLIST."
  ),

  check = function(state) {
    if (inherits(state$spelling, "try-error")) {
      return(list(status = NA, positions = list()))
    }

    res <- state$spelling
    if (nrow(res) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    problems <- list()
    for (i in seq_len(nrow(res))) {
      locations <- res$found[[i]]
      for (loc in locations) {
        parts <- strsplit(loc, ":")[[1]]
        line_nums <- as.integer(strsplit(parts[2], ",")[[1]])
        for (ln in line_nums) {
          problems[[length(problems) + 1]] <- list(
            filename = parts[1],
            line_number = ln,
            column_number = NA_integer_,
            ranges = list(),
            line = res$word[i]
          )
        }
      }
    }

    list(status = FALSE, positions = problems)
  }
)
