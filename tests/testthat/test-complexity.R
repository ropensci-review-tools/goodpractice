# -- complexity_function_length ----------------------------------------------------------

test_that("complexity_function_length fails on long functions", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: longfn", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  body <- paste(paste0("  x", seq_len(60), " <- ", seq_len(60)), collapse = "\n")
  writeLines(
    paste0("big_fn <- function() {\n", body, "\n}"),
    file.path(pkg, "R", "big.R")
  )

  gp_res <- gp(pkg, checks = "complexity_function_length")
  res <- results(gp_res)
  expect_false(res$passed[res$check == "complexity_function_length"])

  pos <- failed_positions(gp_res)$complexity_function_length
  expect_true(any(grepl("big_fn", vapply(pos, `[[`, "", "line"))))
})

test_that("complexity_function_length passes on short functions", {
  gp_res <- gp("good", checks = "complexity_function_length")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "complexity_function_length"])
})

test_that("complexity_function_length respects custom limit option", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: customlimit", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  body <- paste(paste0("  x", seq_len(10), " <- ", seq_len(10)), collapse = "\n")
  writeLines(
    paste0("medium_fn <- function() {\n", body, "\n}"),
    file.path(pkg, "R", "medium.R")
  )

  withr::local_options(goodpractice.function_length_limit = 5L)
  gp_res <- gp(pkg, checks = "complexity_function_length")
  res <- results(gp_res)
  expect_false(res$passed[res$check == "complexity_function_length"])
})

test_that("complexity_function_length passes with no R directory", {
  pkg <- withr::local_tempdir()
  writeLines(c(
    "Package: nodir", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))

  gp_res <- gp(pkg, checks = "complexity_function_length")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "complexity_function_length"])
})

test_that("ts_function_length counts lines correctly", {
  code <- "my_fn <- function(x) {\n  x + 1\n  x + 2\n}"
  lang <- treesitter.r::language()
  p <- treesitter::parser(lang)
  tree <- treesitter::parser_parse(p, code)
  root <- treesitter::tree_root_node(tree)
  fns <- ts_file_functions(root, "test.R")
  expect_equal(ts_function_length(fns[[1]]$fn_node), 4L)
})

test_that("ts_all_referenced_functions returns empty for no trees", {
  pkg <- withr::local_tempdir()
  ts <- ts_parse(pkg)
  expect_equal(ts_all_referenced_functions(ts), character())
})

test_that("ts_all_referenced_functions extracts call names", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines("f <- function() { mean(1:10) }", file.path(pkg, "R", "code.R"))
  ts <- ts_parse(pkg)
  calls <- ts_all_referenced_functions(ts)
  expect_true("mean" %in% calls)
})

# -- complexity_unused_internal ------------------------------------------------

test_that("complexity_unused_internal fails on dead code", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: deadcode", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines("export(public_fn)", file.path(pkg, "NAMESPACE"))

  writeLines(c(
    "public_fn <- function() helper()",
    "helper <- function() 42",
    "dead_fn <- function() 99"
  ), file.path(pkg, "R", "code.R"))

  gp_res <- gp(pkg, checks = "complexity_unused_internal")
  res <- results(gp_res)
  expect_false(res$passed[res$check == "complexity_unused_internal"])

  pos <- failed_positions(gp_res)$complexity_unused_internal
  names <- vapply(pos, `[[`, "", "line")
  expect_true("dead_fn" %in% names)
  expect_false("helper" %in% names)
})

test_that("complexity_unused_internal passes when all used", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: allused", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines("export(public_fn)", file.path(pkg, "NAMESPACE"))

  writeLines(c(
    "public_fn <- function() helper()",
    "helper <- function() 42"
  ), file.path(pkg, "R", "code.R"))

  gp_res <- gp(pkg, checks = "complexity_unused_internal")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "complexity_unused_internal"])
})

test_that("complexity_unused_internal passes with no functions", {
  gp_res <- gp("good", checks = "complexity_unused_internal")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "complexity_unused_internal"])
})

test_that("complexity_unused_internal passes when all exported", {
  pkg <- withr::local_tempdir()
  pkg <- file.path(pkg, "allexport")
  dir.create(pkg)
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: allexport", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines("export(my_fn)", file.path(pkg, "NAMESPACE"))

  writeLines("my_fn <- function() 1", file.path(pkg, "R", "code.R"))

  gp_res <- gp(pkg, checks = "complexity_unused_internal")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "complexity_unused_internal"])
})

test_that("unused_internal detects functions passed as arguments", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: hoftest", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines("export(public_fn)", file.path(pkg, "NAMESPACE"))

  writeLines(c(
    "public_fn <- function(x) lapply(x, helper)",
    "helper <- function(i) i + 1",
    "dead_fn <- function() 99"
  ), file.path(pkg, "R", "code.R"))

  gp_res <- gp(pkg, checks = "complexity_unused_internal")
  pos <- failed_positions(gp_res)$complexity_unused_internal
  names <- vapply(pos, `[[`, "", "line")
  expect_false("helper" %in% names)
  expect_true("dead_fn" %in% names)
})

test_that("unused_internal detects functions assigned to lists", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: listtest", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines("export(public_fn)", file.path(pkg, "NAMESPACE"))

  writeLines(c(
    "MY_LIST <- list()",
    "public_fn <- function() MY_LIST[[1]](42)",
    "prep_fn <- function(x) x * 2",
    "MY_LIST$prep <- prep_fn"
  ), file.path(pkg, "R", "code.R"))

  gp_res <- gp(pkg, checks = "complexity_unused_internal")
  pos <- failed_positions(gp_res)$complexity_unused_internal
  if (is.null(pos)) pos <- list()
  names <- vapply(pos, `[[`, "", "line")
  expect_false("prep_fn" %in% names)
})
