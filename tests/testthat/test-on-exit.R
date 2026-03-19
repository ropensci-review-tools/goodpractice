test_that("on_exit_has_add fails when add is missing", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: badonexit", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(
    "cleanup <- function() { on.exit(close(con)) }",
    file.path(pkg, "R", "cleanup.R")
  )

  gp_res <- gp(pkg, checks = "on_exit_has_add")
  res <- results(gp_res)
  expect_false(res$passed[res$check == "on_exit_has_add"])
})

test_that("on_exit_has_add passes when add = TRUE", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: goodonexit", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "R/../DESCRIPTION"))
  writeLines(
    "cleanup <- function() { on.exit(close(con), add = TRUE) }",
    file.path(pkg, "R", "cleanup.R")
  )

  gp_res <- gp(pkg, checks = "on_exit_has_add")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "on_exit_has_add"])
})

test_that("on_exit_has_add passes when add = FALSE (explicitly set)", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: explicitfalse", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(
    "cleanup <- function() { on.exit(close(con), add = FALSE) }",
    file.path(pkg, "R", "cleanup.R")
  )

  gp_res <- gp(pkg, checks = "on_exit_has_add")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "on_exit_has_add"])
})

test_that("on_exit_has_add passes when on.exit() has no args (reset)", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: resetonexit", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(
    "cleanup <- function() { on.exit() }",
    file.path(pkg, "R", "cleanup.R")
  )

  gp_res <- gp(pkg, checks = "on_exit_has_add")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "on_exit_has_add"])
})

test_that("on_exit_has_add passes with no functions", {
  gp_res <- gp("good", checks = "on_exit_has_add")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "on_exit_has_add"])
})
