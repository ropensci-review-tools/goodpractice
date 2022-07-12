
test_that("importing package as a whole is not okay", {

  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "no_import_package_as_a_whole")
  expect_false(results(x)$result)

  x <- gp("bad2", checks = "no_import_package_as_a_whole")
  expect_true(results(x)$result)

})

test_that("exportPattern is not okay", {

  # TODO expectation/example for "no_export_pattern" failing

  x <- gp("bad2", checks = "no_export_pattern")
  expect_true(results(x)$result)

})
