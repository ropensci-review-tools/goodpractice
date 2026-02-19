bad_cleanup <- function() {
  old <- options(warn = 2)
  on.exit(options(old))
  TRUE
}

good_cleanup <- function() {
  old <- options(warn = 2)
  on.exit(options(old), add = TRUE)
  TRUE
}

no_restore <- function() {
  old <- par(mar = c(1, 1, 1, 1))
  plot(1:10)
}

also_no_restore <- function() {
  setwd("/tmp")
  readLines("data.txt")
}
