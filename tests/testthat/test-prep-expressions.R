test_that("package_collate returns NULL when no Collate field", {
  result <- package_collate("good")
  expect_null(result)
})

test_that("package_collate returns file list when Collate exists", {
  pkg <- withr::local_tempdir("collate_pkg")
  dir.create(file.path(pkg, "R"))
  writeLines("f <- function() 1", file.path(pkg, "R", "f.R"))
  writeLines(
    c(
      "Package: collatepkg",
      "Title: Test",
      "Version: 0.0.1",
      "Description: Test.",
      "Collate: 'f.R'"
    ),
    file.path(pkg, "DESCRIPTION")
  )

  result <- package_collate(pkg)
  expect_equal(result, "f.R")
})

test_that("r_package_files returns R files for fixture package", {
  files <- r_package_files("good")
  expect_true(length(files) > 0)
  expect_true(all(grepl("\\.R$", files, ignore.case = TRUE)))
})

test_that("r_package_files includes full path", {
  files <- r_package_files("bad_tags")
  expect_true(all(grepl("bad_tags/R/", files)))
})

test_that("prep_expressions populates state$expressions", {
  state <- list(path = "good")
  result <- prep_expressions(state, quiet = TRUE)
  expect_true("expressions" %in% names(result))
  expect_true(length(result$expressions) > 0)
})
