
test_that("T and F are not okay", {

  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "truefalse_not_tf")
  expect_false(results(x)$result)

  x <- gp("bad2", checks = "truefalse_not_tf")
  expect_true(results(x)$result)

})
