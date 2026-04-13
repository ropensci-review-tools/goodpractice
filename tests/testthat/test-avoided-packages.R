test_that("no_obsolete_deps fails when DESCRIPTION depends on obsolete pkg", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(
    c(
      "Package: badpkg",
      "Title: Test",
      "Version: 1.0.0",
      "Description: Test.",
      "License: MIT",
      "Imports: sp"
    ),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines("f <- function() 1", file.path(pkg, "R", "code.R"))

  gp_res <- gp(pkg, checks = "no_obsolete_deps")
  res <- results(gp_res)
  expect_false(res$passed[res$check == "no_obsolete_deps"])

  pos <- failed_positions(gp_res)$no_obsolete_deps
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("sp", lines)))
})

test_that("no_obsolete_deps passes with no obsolete dependencies", {
  gp_res <- gp("good", checks = "no_obsolete_deps")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "no_obsolete_deps"])
})

test_that("no_obsolete_deps catches multiple obsolete packages", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(
    c(
      "Package: badpkg2",
      "Title: Test",
      "Version: 1.0.0",
      "Description: Test.",
      "License: MIT",
      "Imports: RCurl, XML"
    ),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines("f <- function() 1", file.path(pkg, "R", "code.R"))

  gp_res <- gp(pkg, checks = "no_obsolete_deps")
  res <- results(gp_res)
  expect_false(res$passed[res$check == "no_obsolete_deps"])

  pos <- failed_positions(gp_res)$no_obsolete_deps
  expect_length(pos, 2)
})

test_that("no_obsolete_deps returns NA on description error", {
  state <- list(description = structure("error", class = "try-error"))
  result <- CHECKS$no_obsolete_deps$check(state)
  expect_true(is.na(result$status))
})

test_that("no_obsolete_deps gp message lists found packages", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(
    c(
      "Package: msgtest",
      "Title: Test",
      "Version: 1.0.0",
      "Description: Test.",
      "License: MIT",
      "Imports: rjson"
    ),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines("f <- function() 1", file.path(pkg, "R", "code.R"))

  gp_res <- gp(pkg, checks = "no_obsolete_deps")
  chk <- CHECKS$no_obsolete_deps
  msg <- chk$gp(gp_res)
  expect_match(msg, "rjson")
  expect_match(msg, "jsonlite")
})
