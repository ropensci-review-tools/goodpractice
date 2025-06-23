
test_that("warning if prep step fails", {
  bad1 <- system.file("bad1", package = "goodpractice")

  expect_warning(gp(bad1, checks = "covr"),
                 "Prep step for test coverage failed.")
})
