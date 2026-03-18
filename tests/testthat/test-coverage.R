
test_that("warning if prep step fails", {
  bad1 <- system.file("bad1", package = "goodpractice")

  expect_warning(gp(bad1, checks = "covr"),
                 "Prep step for.*covr.*failed")
})

test_that("run_prep_step passes multiple ... args to fn", {
  state <- list()
  state <- run_prep_step(state, "test", function(a, b, c) {
    list(sum = a + b + c)
  }, a = 1, b = 2, c = 3)
  expect_equal(state$test$sum, 6)
})
