good_func <- function(x, y) {
  if (is.na(x) || y > 0) {
    x
  } else {
    x + y
  }
}
