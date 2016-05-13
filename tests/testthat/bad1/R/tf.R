
tf <- function() {
  if (TRUE == T) print("foo")
  T + F + 1
}

## These shuld be OK (but it's not currently)

tfgood <- function() {
  T <- "yes!"
  print(T)
}
