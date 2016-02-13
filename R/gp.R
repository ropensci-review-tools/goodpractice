
gp <- function(checks = all_checks()) {

  preps <- unique(unlist(lapply(CHECKS[checks], "[[", "preps")))

  state <- list()

  for (prep in names(PREPS)) state <- PREPS[[prep]](state)

  gp_header()

  for (check in checks) {
    if (! CHECKS[[check]]$check(state)) gp_advice(CHECKS[[check]]$gp)
  }
}

gp_header <- function() {
  cat("--------------------------------------------------------------\n")
  cat("It is good practice to\n\n")
}

gp_advice <- function(str) {
  str <- gsub("\n\\s*", " ", str)
  str <- paste(
    strwrap(paste0("* ", str), indent = 2, exdent = 4),
    collapse = "\n"
  )
  cat(str, "\n\n")
}
