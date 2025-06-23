
test_that("print with default and explicit positions.limit", {
  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "truefalse_not_tf")

  testthat::expect_snapshot (print (x))
})
