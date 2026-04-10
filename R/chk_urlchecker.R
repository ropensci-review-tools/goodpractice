#' @include lists.R

urlchecker_make_positions <- function(db) {
  lapply(seq_len(nrow(db)), function(i) {
    from <- db$From[[i]]
    check_position(
      if (length(from) > 0) from[[1]] else "unknown",
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
        return(na_result())
      }

      db <- state$urlchecker
      if (is.null(db) || nrow(db) == 0) {
        return(check_result(TRUE))
      }

      problems <- filter(db)
      if (nrow(problems) == 0) {
        return(check_result(TRUE))
      }

      check_result(FALSE, urlchecker_make_positions(problems))
    }
  )
}

CHECKS$urlchecker_ok <- make_urlchecker_check(
  description = "All URLs are reachable",
  gp = "Fix or remove broken URLs in documentation and {.file DESCRIPTION}.",
  filter = function(db) db[!db$Status %in% c("200", "301", "302"), ]
)

CHECKS$urlchecker_no_redirects <- make_urlchecker_check(
  description = "No URLs redirect to a different location",
  tags = "CRAN",
  gp = "Update URLs that redirect to their final destination.",
  filter = function(db) db[nzchar(db$New), ]
)
