test_that("excluded_paths returns empty by default", {
  withr::local_options(goodpractice.exclude_path = NULL)
  withr::local_envvar(GP_EXCLUDE_PATH = "")
  expect_identical(excluded_paths(), character(0))
})

test_that("excluded_paths reads from option", {
  withr::local_options(goodpractice.exclude_path = c("R/a.R", "R/b.R"))
  result <- excluded_paths()
  expect_equal(result, c("R/a.R", "R/b.R"))
})

test_that("excluded_paths reads from envvar", {
  withr::local_options(goodpractice.exclude_path = NULL)
  withr::local_envvar(GP_EXCLUDE_PATH = "R/a.R,R/b.R")
  result <- excluded_paths()
  expect_equal(result, c("R/a.R", "R/b.R"))
})

test_that("excluded_paths option takes precedence over envvar", {
  withr::local_options(goodpractice.exclude_path = "R/from_option.R")
  withr::local_envvar(GP_EXCLUDE_PATH = "R/from_envvar.R")
  expect_equal(excluded_paths(), "R/from_option.R")
})

test_that("filter_excluded_paths removes matching files", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "R"))
  file.create(file.path(tmp, "R", "a.R"))
  file.create(file.path(tmp, "R", "b.R"))
  file.create(file.path(tmp, "R", "c.R"))

  files <- file.path(tmp, "R", c("a.R", "b.R", "c.R"))
  result <- filter_excluded_paths(files, tmp, "R/a.R")
  expect_equal(basename(result), c("b.R", "c.R"))
})

test_that("filter_excluded_paths with empty exclude returns all files", {
  files <- c("/tmp/R/a.R", "/tmp/R/b.R")
  result <- filter_excluded_paths(files, "/tmp", character())
  expect_equal(result, files)
})

test_that("gp passes exclude_path to state", {
  withr::local_options(goodpractice.exclude_path = "R/functions.R")
  pkg <- system.file("bad1", package = "goodpractice")
  g <- gp(pkg, checks = "print_return_invisible")
  res <- results(g)
  expect_true("print_return_invisible" %in% res$check)
  expect_equal(g$exclude_path, "R/functions.R")
})
