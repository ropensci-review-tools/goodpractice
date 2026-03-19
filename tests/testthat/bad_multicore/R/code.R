library(multicore)

do_parallel <- function(x) {
  multicore::mclapply(x, identity)
}
