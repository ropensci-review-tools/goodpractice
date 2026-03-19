test_that("roxygen2_duplicate_params fails on identical params across files", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: dupepkg", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  writeLines(c(
    "#' Function A",
    "#' @param x A numeric value.",
    "#' @export",
    "func_a <- function(x) x + 1"
  ), file.path(pkg, "R", "a.R"))

  writeLines(c(
    "#' Function B",
    "#' @param x A numeric value.",
    "#' @export",
    "func_b <- function(x) x * 2"
  ), file.path(pkg, "R", "b.R"))

  state <- list(path = pkg)
  result <- CHECKS$roxygen2_duplicate_params$check(state)
  expect_false(result$status)
  expect_true(length(result$positions) >= 2)
  lines <- vapply(result$positions, `[[`, "", "line")
  expect_true(all(grepl("@param x", lines)))
})

test_that("roxygen2_duplicate_params passes with different descriptions", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: nodupepkg", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  writeLines(c(
    "#' Function A",
    "#' @param x A numeric input.",
    "#' @export",
    "func_a <- function(x) x + 1"
  ), file.path(pkg, "R", "a.R"))

  writeLines(c(
    "#' Function B",
    "#' @param x A character string.",
    "#' @export",
    "func_b <- function(x) paste(x)"
  ), file.path(pkg, "R", "b.R"))

  state <- list(path = pkg)
  result <- CHECKS$roxygen2_duplicate_params$check(state)
  expect_true(result$status)
})

test_that("roxygen2_duplicate_params passes with no R directory", {
  pkg <- withr::local_tempdir()
  state <- list(path = pkg)
  result <- CHECKS$roxygen2_duplicate_params$check(state)
  expect_true(result$status)
})

test_that("roxygen2_duplicate_params ignores duplicates within same file", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: samefile", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  writeLines(c(
    "#' Function A",
    "#' @param x A numeric value.",
    "#' @export",
    "func_a <- function(x) x + 1",
    "",
    "#' Function B",
    "#' @param x A numeric value.",
    "#' @export",
    "func_b <- function(x) x * 2"
  ), file.path(pkg, "R", "funcs.R"))

  state <- list(path = pkg)
  result <- CHECKS$roxygen2_duplicate_params$check(state)
  expect_true(result$status)
})

test_that("roxygen2_duplicate_params handles multiline descriptions", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: multiline", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  writeLines(c(
    "#' Function A",
    "#' @param x A numeric value that",
    "#'   represents the input.",
    "#' @export",
    "func_a <- function(x) x + 1"
  ), file.path(pkg, "R", "a.R"))

  writeLines(c(
    "#' Function B",
    "#' @param x A numeric value that",
    "#'   represents the input.",
    "#' @export",
    "func_b <- function(x) x * 2"
  ), file.path(pkg, "R", "b.R"))

  state <- list(path = pkg)
  result <- CHECKS$roxygen2_duplicate_params$check(state)
  expect_false(result$status)
})
