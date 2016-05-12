
att <- function() {
  attach(iris)
  Sepal.Width
  detach(iris)
}

sw <- function() {
  setwd("foobar")
}

sapp <- function() {
  sapply(1:10, print)
  sapply(list(), print)
}

lib <- function() {
  library(igraph)
  require(pkgconfig)
}
