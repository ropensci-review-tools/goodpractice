
f <- function(x) {

  ## This is bad practice, oh no!
  for (i in 1:length(x)) i

  for (i in 1:nrow(x)) i

  for (i in 1:ncol(x)) i

  for (i in 1:NROW(x)) i

  for (i in 1:NCOL(x)) i
}
