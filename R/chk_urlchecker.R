#' @include lists.R

urlchecker_na_result <- function() {
  list(status = NA, positions = list())
}

urlchecker_make_positions <- function(db) {
  lapply(seq_len(nrow(db)), function(i) {
    from <- db$From[[i]]
    list(
      filename = if (length(from) > 0) from[[1]] else "unknown",
      line_number = NA_integer_,
      column_number = NA_integer_,
      ranges = list(),
      line = db$URL[i]
    )
  })
}

make_urlchecker_check <- function(description, gp, filter, tags = NULL) {
  make_check(
    description = description,
    tags = c("documentation", "url", tags),
    preps = "urlchecker",
    gp = gp,

    check = function(state) {
      if (inherits(state$urlchecker, "try-error")) {
        return(urlchecker_na_result())
      }

      db <- state$urlchecker
      if (is.null(db) || nrow(db) == 0) {
        return(list(status = TRUE, positions = list()))
      }

      problems <- filter(db)
      if (nrow(problems) == 0) {
        return(list(status = TRUE, positions = list()))
      }

      list(
        status = FALSE,
        positions = urlchecker_make_positions(problems)
      )
    }
  )
}

CHECKS$urlchecker_ok <- make_urlchecker_check(
  description = "All URLs are reachable",
  gp = "Fix or remove broken URLs in documentation and DESCRIPTION.",
  filter = function(db) db[!db$Status %in% c("200", "301", "302"), ]
)

CHECKS$urlchecker_no_redirects <- make_urlchecker_check(
  description = "No URLs redirect to a different location",
  tags = "CRAN",
  gp = "Update URLs that redirect to their final destination.",
  filter = function(db) db[nzchar(db$New), ]
)
