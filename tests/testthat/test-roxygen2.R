get_result <- function(res, check) res$result[res$check == check]

# -- uses_roxygen2 detection --------------------------------------------------

test_that("uses_roxygen2 returns TRUE for roxygen2 packages", {
  expect_true(uses_roxygen2("bad_roxygen"))
})

test_that("uses_roxygen2 returns FALSE for non-roxygen2 packages", {
  expect_false(uses_roxygen2("no_roxygen"))
})

test_that("uses_roxygen2 returns FALSE for missing DESCRIPTION", {
  expect_false(uses_roxygen2(tempfile()))
})

# -- prep skips non-roxygen2 packages -----------------------------------------

test_that("roxygen2 checks return NA for non-roxygen2 packages", {
  expect_warning(
    gp_res <- gp("no_roxygen", checks = "roxygen2_unknown_tags"),
    "Prep step for roxygen2 failed"
  )
  res <- results(gp_res)
  expect_true(is.na(get_result(res, "roxygen2_unknown_tags")))
})

# -- roxygen2_has_export_or_nord ----------------------------------------------

test_that("roxygen2_has_export_or_nord fails for untagged functions", {
  gp_res <- gp("bad_roxygen", checks = "roxygen2_has_export_or_nord")
  res <- results(gp_res)
  expect_false(get_result(res, "roxygen2_has_export_or_nord"))

  pos <- failed_positions(gp_res)$roxygen2_has_export_or_nord
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("untagged_func", lines)))
})

test_that("roxygen2_has_export_or_nord passes when all tagged", {
  gp_res <- gp("good", checks = "roxygen2_has_export_or_nord")
  res <- results(gp_res)
  expect_true(get_result(res, "roxygen2_has_export_or_nord"))
})

# -- roxygen2_nord_has_keywords_internal --------------------------------------

test_that("roxygen2_nord_has_keywords_internal fails when @noRd lacks @keywords", {
  gp_res <- gp("bad_roxygen", checks = "roxygen2_nord_has_keywords_internal")
  res <- results(gp_res)
  expect_false(get_result(res, "roxygen2_nord_has_keywords_internal"))

  pos <- failed_positions(gp_res)$roxygen2_nord_has_keywords_internal
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("internal_no_keywords", lines)))
})

test_that("roxygen2_nord_has_keywords_internal passes when all @noRd have @keywords", {
  gp_res <- gp("good", checks = "roxygen2_nord_has_keywords_internal")
  res <- results(gp_res)
  expect_true(get_result(res, "roxygen2_nord_has_keywords_internal"))
})

# -- roxygen2_no_export_and_keywords_internal ---------------------------------

test_that("roxygen2_no_export_and_keywords_internal fails on conflict", {
  gp_res <- gp("bad_roxygen", checks = "roxygen2_no_export_and_keywords_internal")
  res <- results(gp_res)
  expect_false(get_result(res, "roxygen2_no_export_and_keywords_internal"))

  pos <- failed_positions(gp_res)$roxygen2_no_export_and_keywords_internal
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("confused_func", lines)))
})

test_that("roxygen2_no_export_and_keywords_internal passes when consistent", {
  gp_res <- gp("good", checks = "roxygen2_no_export_and_keywords_internal")
  res <- results(gp_res)
  expect_true(get_result(res, "roxygen2_no_export_and_keywords_internal"))
})

# -- roxygen2_unknown_tags ----------------------------------------------------

test_that("roxygen2_unknown_tags fails on deprecated/unknown tags", {
  gp_res <- gp("bad_roxygen", checks = "roxygen2_unknown_tags")
  res <- results(gp_res)
  expect_false(get_result(res, "roxygen2_unknown_tags"))

  pos <- failed_positions(gp_res)$roxygen2_unknown_tags
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("@S3method", lines)))
})

test_that("roxygen2_unknown_tags passes when all tags are valid", {
  gp_res <- gp("good", checks = "roxygen2_unknown_tags")
  res <- results(gp_res)
  expect_true(get_result(res, "roxygen2_unknown_tags"))
})

# -- roxygen2_valid_inherit ---------------------------------------------------

test_that("roxygen2_valid_inherit fails on nonexistent reference", {
  gp_res <- gp("bad_roxygen", checks = "roxygen2_valid_inherit")
  res <- results(gp_res)
  expect_false(get_result(res, "roxygen2_valid_inherit"))

  pos <- failed_positions(gp_res)$roxygen2_valid_inherit
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("bad_inherit_func", lines)))
})

test_that("roxygen2_valid_inherit passes with valid references", {
  gp_res <- gp("good", checks = "roxygen2_valid_inherit")
  res <- results(gp_res)
  expect_true(get_result(res, "roxygen2_valid_inherit"))
})

# -- prep error handling ------------------------------------------------------

test_that("roxygen2 checks return NA on prep failure", {
  state <- list(roxygen2 = structure("error", class = "try-error"))
  result <- CHECKS$roxygen2_unknown_tags$check(state)
  expect_true(is.na(result$status))
})

# -- block_is_function edge cases ---------------------------------------------

test_that("block_is_function returns FALSE for non-assignment operator", {
  block <- list(call = quote(x + y))
  expect_false(block_is_function(block))
})

test_that("block_is_function returns FALSE when LHS is not a name", {
  block <- list(call = quote(x$y <- function() {}))
  expect_false(block_is_function(block))
})

# -- find_function_defs edge cases --------------------------------------------

test_that("find_function_defs returns empty data.frame when no functions found", {
  pkg <- withr::local_tempdir("no_fns")
  dir.create(file.path(pkg, "R"))
  writeLines("x <- 1", file.path(pkg, "R", "data.R"))
  writeLines(
    c("Package: nofnspkg", "Title: Test", "Version: 1.0.0",
      "Description: Test.", "License: MIT"),
    file.path(pkg, "DESCRIPTION")
  )

  result <- find_function_defs(pkg)
  expect_equal(nrow(result), 0)
  expect_true(all(c("name", "file", "line") %in% names(result)))
})

test_that("find_function_defs skips non-existent files", {
  pkg <- withr::local_tempdir("missing_file")
  dir.create(file.path(pkg, "R"))
  writeLines("my_fn <- function() 1", file.path(pkg, "R", "real.R"))
  writeLines(
    c("Package: missingpkg", "Title: Test", "Version: 1.0.0",
      "Description: Test.", "License: MIT",
      "Collate: 'ghost.R' 'real.R'"),
    file.path(pkg, "DESCRIPTION")
  )

  result <- find_function_defs(pkg)
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "my_fn")
})

# -- parse_roxygen2 NAMESPACE error fallback ----------------------------------

test_that("parse_roxygen2 handles broken NAMESPACE gracefully", {
  pkg <- withr::local_tempdir("broken_ns")
  dir.create(file.path(pkg, "R"))
  writeLines(
    c("#' A function", "#' @export", "hello <- function() 1"),
    file.path(pkg, "R", "hello.R")
  )
  writeLines(
    c("Package: brokenns", "Title: Test", "Version: 1.0.0",
      "Description: Test.", "License: MIT", "RoxygenNote: 7.3.3"),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines("this is not valid namespace content!!!", file.path(pkg, "NAMESPACE"))

  result <- parse_roxygen2(pkg)
  expect_equal(result$namespace_exports, character())
  expect_equal(result$namespace_s3methods, character())
})

# -- parse_roxygen2 S3 methods ------------------------------------------------

test_that("parse_roxygen2 extracts S3 methods from NAMESPACE", {
  pkg <- withr::local_tempdir("s3methods")
  dir.create(file.path(pkg, "R"))
  writeLines(
    c("#' Print method", "#' @export", "print.myclass <- function(x, ...) x"),
    file.path(pkg, "R", "print.R")
  )
  writeLines(
    c("Package: s3pkg", "Title: Test", "Version: 1.0.0",
      "Description: Test.", "License: MIT", "RoxygenNote: 7.3.3"),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines(
    c("# Generated by roxygen2: do not edit by hand",
      "S3method(print,myclass)"),
    file.path(pkg, "NAMESPACE")
  )

  result <- parse_roxygen2(pkg)
  expect_true("print.myclass" %in% result$namespace_s3methods)
})
