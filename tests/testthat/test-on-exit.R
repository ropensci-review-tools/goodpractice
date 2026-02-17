get_result <- function(res, check) res$result[res$check == check]

test_that("on_exit_add fails when on.exit() lacks add = TRUE", {
  gp_res <- gp("bad_on_exit", checks = "on_exit_add")
  res <- results(gp_res)
  expect_false(get_result(res, "on_exit_add"))

  pos <- failed_positions(gp_res)$on_exit_add
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("bad_cleanup", lines)))
  expect_true(any(grepl("without add = TRUE", lines)))
})

test_that("on_exit_add passes when all on.exit() calls have add", {
  gp_res <- gp("good", checks = "on_exit_add")
  res <- results(gp_res)
  expect_true(get_result(res, "on_exit_add"))
})

test_that("on_exit_add ignores empty on.exit() calls", {
  pkg <- file.path(tempdir(), "emptyon")
  dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("Package: emptyon", "Title: Test", "Version: 0.1.0",
      "Description: Test.", "License: MIT"),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines(
    c("reset_handler <- function() {", "  on.exit()", "  TRUE", "}"),
    file.path(pkg, "R", "reset.R")
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)

  state <- list(path = pkg)
  state <- PREPS$on_exit(state, quiet = TRUE)
  result <- CHECKS$on_exit_add$check(state)
  expect_true(result$status)
  expect_length(result$positions, 0)
})

# -- on_exit_missing -----------------------------------------------------------

test_that("on_exit_missing fails when state-changing calls lack on.exit()", {
  gp_res <- gp("bad_on_exit", checks = "on_exit_missing")
  res <- results(gp_res)
  expect_false(get_result(res, "on_exit_missing"))

  pos <- failed_positions(gp_res)$on_exit_missing
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("no_restore", lines)))
  expect_true(any(grepl("par", lines)))
  expect_true(any(grepl("also_no_restore", lines)))
  expect_true(any(grepl("setwd", lines)))
})

test_that("on_exit_missing passes when all state changes have on.exit()", {
  gp_res <- gp("good", checks = "on_exit_missing")
  res <- results(gp_res)
  expect_true(get_result(res, "on_exit_missing"))
})

test_that("on_exit_missing does not flag functions without state changes", {
  pkg <- file.path(tempdir(), "nostate")
  dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("Package: nostate", "Title: Test", "Version: 0.1.0",
      "Description: Test.", "License: MIT"),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines(
    c("pure_func <- function(x) {", "  x + 1", "}"),
    file.path(pkg, "R", "pure.R")
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)

  state <- list(path = pkg)
  state <- PREPS$on_exit(state, quiet = TRUE)
  result <- CHECKS$on_exit_missing$check(state)
  expect_true(result$status)
  expect_length(result$positions, 0)
})

test_that("on_exit_missing ignores state changes in nested functions", {
  pkg <- file.path(tempdir(), "nested_on_exit")
  dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("Package: nestedonexit", "Title: Test", "Version: 0.1.0",
      "Description: Test.", "License: MIT"),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines(
    c(
      "outer <- function() {",
      "  on.exit(options(old), add = TRUE)",
      "  old <- options(warn = 2)",
      "  inner <- function() {",
      "    sink('file.txt')",
      "  }",
      "}"
    ),
    file.path(pkg, "R", "nested.R")
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)

  state <- list(path = pkg)
  state <- PREPS$on_exit(state, quiet = TRUE)

  outer_row <- state$on_exit$state_changers[
    state$on_exit$state_changers$name == "outer",
  ]
  expect_true(outer_row$has_on_exit)
})

# -- helper functions ----------------------------------------------------------

test_that("find_calls_shallow handles recursive non-call nodes", {
  pl <- pairlist(a = quote(options(x = 1)), b = quote(par(mar = c(1, 1, 1, 1))))
  result <- find_calls_shallow(pl, c("options", "par"))
  expect_true("options" %in% result)
  expect_true("par" %in% result)
})

test_that("find_on_exit_calls handles recursive non-call nodes", {
  pl <- pairlist(a = quote(on.exit(options(old))), b = 1L)
  result <- find_on_exit_calls(pl)
  expect_length(result, 1)
  expect_false(result[[1]]$has_add)
})
