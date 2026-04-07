
test_that("namespace checks return NA on try-error state", {
  state <- list(namespace = structure("error", class = "try-error",
    condition = simpleError("test")))

  for (nm in c("no_import_package_as_a_whole", "no_export_pattern")) {
    result <- CHECKS[[nm]]$check(state)
    expect_identical(result$status, NA, label = paste(nm, "status on try-error"))
    expect_identical(result$positions, list(), label = paste(nm, "positions on try-error"))
  }
})

test_that("no_import_package_as_a_whole returns list with status and positions", {
  state <- list(namespace = list(
    imports = list(c("pkg1", "fun1"), c("pkg2", "fun2", "fun3"))
  ))
  result <- CHECKS$no_import_package_as_a_whole$check(state)
  expect_true(result$status)
  expect_identical(result$positions, list())

  state <- list(namespace = list(
    imports = list(c("pkg1", "fun1"), "pkg2")
  ))
  result <- CHECKS$no_import_package_as_a_whole$check(state)
  expect_false(result$status)
  expect_identical(result$positions, list())
})

test_that("no_export_pattern returns list with status and positions", {
  state <- list(namespace = list(exportPatterns = character()))
  result <- CHECKS$no_export_pattern$check(state)
  expect_true(result$status)
  expect_identical(result$positions, list())

  state <- list(namespace = list(exportPatterns = "^[^.]"))
  result <- CHECKS$no_export_pattern$check(state)
  expect_false(result$status)
  expect_identical(result$positions, list())
})

test_that("importing package as a whole is not okay", {

  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "no_import_package_as_a_whole")
  expect_false(results(x)$passed)

  x <- gp("bad2", checks = "no_import_package_as_a_whole")
  expect_true(results(x)$passed)

})

test_that("exportPattern is not okay", {

  # TODO expectation/example for "no_export_pattern" failing

  x <- gp("bad2", checks = "no_export_pattern")
  expect_true(results(x)$passed)

})
