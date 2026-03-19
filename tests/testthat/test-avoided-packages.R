test_that("no_import_multicore fails on library(multicore)", {
  gp_res <- gp("bad_multicore", checks = "no_import_multicore")
  res <- results(gp_res)
  expect_false(res$passed[res$check == "no_import_multicore"])

  pos <- failed_positions(gp_res)$no_import_multicore
  expect_true(length(pos) >= 2)
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("library", lines)))
  expect_true(any(grepl("multicore::", lines)))
})

test_that("no_import_multicore passes when multicore not used", {
  gp_res <- gp("good", checks = "no_import_multicore")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "no_import_multicore"])
})

test_that("avoided package checks pass when no R directory", {
  pkg <- withr::local_tempdir()
  writeLines(c(
    "Package: nopkg", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  gp_res <- gp(pkg, checks = "no_import_multicore")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "no_import_multicore"])
})

test_that("all avoided package checks are registered", {
  expected <- paste0("no_import_", tolower(names(AVOIDED_PACKAGES)))
  for (name in expected) {
    expect_true(name %in% names(CHECKS), info = paste(name, "not registered"))
  }
})

test_that("no_import_xml detects XML:: namespace usage", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: badxml", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(
    "parse_it <- function(f) XML::xmlParse(f)",
    file.path(pkg, "R", "parse.R")
  )

  gp_res <- gp(pkg, checks = "no_import_xml")
  res <- results(gp_res)
  expect_false(res$passed[res$check == "no_import_xml"])
})

test_that("no_import_sp detects library(sp)", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: badsp", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines("library(sp)", file.path(pkg, "R", "spatial.R"))

  gp_res <- gp(pkg, checks = "no_import_sp")
  res <- results(gp_res)
  expect_false(res$passed[res$check == "no_import_sp"])
})

test_that("find_avoided_package_usage returns empty for no matches", {
  ts <- ts_parse("good")
  problems <- find_avoided_package_usage(ts, "nonexistent_package")
  expect_equal(length(problems), 0)
})
