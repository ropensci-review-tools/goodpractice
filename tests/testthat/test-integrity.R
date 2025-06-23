
test_that("checks are not overwritten", {

  skip_on_cran()

  pkgdir <- file.path("..", "..", "R")
  rfiles <- list.files(pkgdir, full.names = TRUE)
  rlines <- as.character(unlist(lapply(rfiles, readLines)))
  rwords <- unlist(strsplit(rlines, "\\s+"))
  checks <- grep("CHECKS$", rwords, fixed = TRUE, value = TRUE)
  expect_false(any(duplicated(checks)))
})
