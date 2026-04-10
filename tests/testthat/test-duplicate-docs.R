test_that("duplicate_function_bodies fails on identical fns across files", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: dupecode", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  writeLines(
    "helper_a <- function(x) { result <- x + 1; log(result); result }",
    file.path(pkg, "R", "a.R")
  )
  writeLines(
    "helper_b <- function(x) { result <- x + 1; log(result); result }",
    file.path(pkg, "R", "b.R")
  )

  gp_res <- gp(pkg, checks = "duplicate_function_bodies")
  res <- results(gp_res)
  expect_false(res$passed[res$check == "duplicate_function_bodies"])

  pos <- failed_positions(gp_res)$duplicate_function_bodies
  names <- vapply(pos, `[[`, "", "line")
  expect_true("helper_a" %in% names)
  expect_true("helper_b" %in% names)
})

test_that("duplicate_function_bodies passes with different bodies", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: nodupecode", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  writeLines(
    "add_one <- function(x) { x + 1 }",
    file.path(pkg, "R", "a.R")
  )
  writeLines(
    "times_two <- function(x) { x * 2 }",
    file.path(pkg, "R", "b.R")
  )

  gp_res <- gp(pkg, checks = "duplicate_function_bodies")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "duplicate_function_bodies"])
})

test_that("duplicate_function_bodies ignores trivial one-liners", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: trivial", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  writeLines("f1 <- function() NULL", file.path(pkg, "R", "a.R"))
  writeLines("f2 <- function() NULL", file.path(pkg, "R", "b.R"))

  gp_res <- gp(pkg, checks = "duplicate_function_bodies")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "duplicate_function_bodies"])
})

test_that("duplicate_function_bodies ignores duplicates within same file", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: samefile", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  writeLines(c(
    "helper_a <- function(x) { result <- x + 1; log(result); result }",
    "helper_b <- function(x) { result <- x + 1; log(result); result }"
  ), file.path(pkg, "R", "funcs.R"))

  gp_res <- gp(pkg, checks = "duplicate_function_bodies")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "duplicate_function_bodies"])
})
