# -- uses_roxygen2 detection --------------------------------------------------

test_that("uses_roxygen2 returns TRUE for roxygen2 packages", {
  expect_true(uses_roxygen2("bad_roxygen"))
})

test_that("uses_roxygen2 returns FALSE for non-roxygen2 packages", {
  expect_false(uses_roxygen2("no_roxygen"))
})

# -- prep skips non-roxygen2 packages -----------------------------------------

test_that("roxygen2 checks return NA for non-roxygen2 packages", {
  expect_warning(
    gp_res <- gp("no_roxygen", checks = "roxygen2_unknown_tags"),
    "Prep step for"
  )
  expect_true(is.na(results(gp_res)$passed))
})

# -- roxygen2_has_export_or_nord ----------------------------------------------

test_that("roxygen2_has_export_or_nord flags documented functions missing tags", {
  gp_res <- gp("bad_roxygen", checks = "roxygen2_has_export_or_nord")
  expect_false(results(gp_res)$passed)

  pos <- failed_positions(gp_res)$roxygen2_has_export_or_nord
  lines <- vapply(pos, `[[`, "", "line")
  expect_match(lines, "documented_no_tag", all = FALSE)
})

test_that("roxygen2_has_export_or_nord ignores undocumented functions", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: undoctest", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT",
    "Roxygen: list(markdown = TRUE)",
    "RoxygenNote: 7.3.3"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines("export(public_fn)", file.path(pkg, "NAMESPACE"))

  writeLines(c(
    "#' @export",
    "public_fn <- function() 1",
    "",
    "internal_fn <- function() 2"
  ), file.path(pkg, "R", "code.R"))

  gp_res <- gp(pkg, checks = "roxygen2_has_export_or_nord")
  expect_true(results(gp_res)$passed)
})

test_that("roxygen2_has_export_or_nord passes when all tagged", {
  gp_res <- gp("good", checks = "roxygen2_has_export_or_nord")
  expect_true(results(gp_res)$passed)
})

test_that("roxygen2_has_export_or_nord skips @noRd operator functions", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: optest", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT",
    "Roxygen: list(markdown = TRUE)",
    "RoxygenNote: 7.3.3"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines("export(my_fn)", file.path(pkg, "NAMESPACE"))

  writeLines(c(
    "#' @export",
    "my_fn <- function() 1",
    "",
    "#' @noRd",
    "`%op%` <- function(a, b) a + b"
  ), file.path(pkg, "R", "code.R"))

  gp_res <- gp(pkg, checks = "roxygen2_has_export_or_nord")
  expect_true(results(gp_res)$passed)
})


# -- roxygen2_unknown_tags ----------------------------------------------------

test_that("roxygen2_unknown_tags fails on deprecated/unknown tags", {
  gp_res <- gp("bad_roxygen", checks = "roxygen2_unknown_tags")
  expect_false(results(gp_res)$passed)

  pos <- failed_positions(gp_res)$roxygen2_unknown_tags
  lines <- vapply(pos, `[[`, "", "line")
  expect_match(lines, "@S3method", all = FALSE)
})

test_that("roxygen2_unknown_tags passes when all tags are valid", {
  gp_res <- gp("good", checks = "roxygen2_unknown_tags")
  expect_true(results(gp_res)$passed)
})

# -- roxygen2_valid_inherit ---------------------------------------------------

test_that("roxygen2_valid_inherit fails on nonexistent reference", {
  gp_res <- gp("bad_roxygen", checks = "roxygen2_valid_inherit")
  expect_false(results(gp_res)$passed)

  pos <- failed_positions(gp_res)$roxygen2_valid_inherit
  lines <- vapply(pos, `[[`, "", "line")
  expect_match(lines, "bad_inherit_func", all = FALSE)
})

test_that("roxygen2_valid_inherit passes with valid references", {
  gp_res <- gp("good", checks = "roxygen2_valid_inherit")
  expect_true(results(gp_res)$passed)
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

# -- find_function_defs -------------------------------------------------------

test_that("find_function_defs returns data.frame with correct columns", {
  pkg <- withr::local_tempdir("has_fns")
  dir.create(file.path(pkg, "R"))
  writeLines(
    c("alpha <- function(x) x + 1",
      "beta <- function(y) y * 2"),
    file.path(pkg, "R", "fns.R")
  )
  writeLines(
    c("Package: hasfnspkg", "Title: Test", "Version: 1.0.0",
      "Description: Test.", "License: MIT"),
    file.path(pkg, "DESCRIPTION")
  )

  result <- find_function_defs(pkg)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("name", "file", "line"))
  expect_equal(nrow(result), 2)
  expect_true(all(c("alpha", "beta") %in% result$name))
  expect_true(all(grepl("fns\\.R$", result$file)))
  expect_true(is.numeric(result$line))
  expect_equal(result$line[result$name == "alpha"], 1L)
  expect_equal(result$line[result$name == "beta"], 2L)
})

test_that("find_function_defs returns empty data.frame when no R files exist", {
  pkg <- withr::local_tempdir("no_r_files")
  dir.create(file.path(pkg, "R"))
  writeLines(
    c("Package: norfpkg", "Title: Test", "Version: 1.0.0",
      "Description: Test.", "License: MIT"),
    file.path(pkg, "DESCRIPTION")
  )

  result <- find_function_defs(pkg)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("name", "file", "line"))
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
  expect_named(result, c("name", "file", "line"))
})

test_that("find_function_defs skips non-identifier LHS assignments", {
  pkg <- withr::local_tempdir("non_id_lhs")
  dir.create(file.path(pkg, "R"))
  writeLines(
    c("env <- new.env()",
      "env$helper <- function() 1",
      "real_fn <- function(x) x + 1"),
    file.path(pkg, "R", "code.R")
  )
  writeLines(
    c("Package: nonidlhspkg", "Title: Test", "Version: 1.0.0",
      "Description: Test.", "License: MIT"),
    file.path(pkg, "DESCRIPTION")
  )

  result <- find_function_defs(pkg)
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "real_fn")
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

# -- roxygen2_duplicate_params ------------------------------------------------

test_that("roxygen2_duplicate_params fails on identical params across files", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: dupeparam", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT", "RoxygenNote: 7.3.3"
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

  gp_res <- gp(pkg, checks = "roxygen2_duplicate_params")
  expect_false(results(gp_res)$passed)

  pos <- failed_positions(gp_res)$roxygen2_duplicate_params
  lines <- vapply(pos, `[[`, "", "line")
  expect_match(lines, "@param x", all = TRUE)
})

test_that("roxygen2_duplicate_params passes with different descriptions", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(c(
    "Package: nodupe", "Title: Test", "Version: 1.0.0",
    "Description: Test.", "License: MIT", "RoxygenNote: 7.3.3"
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

  gp_res <- gp(pkg, checks = "roxygen2_duplicate_params")
  expect_true(results(gp_res)$passed)
})

test_that("roxygen2_duplicate_params passes for non-roxygen2 packages", {
  expect_warning(
    gp_res <- gp("no_roxygen", checks = "roxygen2_duplicate_params"),
    "Prep step for"
  )
  expect_true(is.na(results(gp_res)$passed))
})
