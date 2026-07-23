test_that("checks are not overwritten", {
  skip_on_cran()

  pkgdir <- file.path("..", "..", "R")
  rfiles <- list.files(pkgdir, full.names = TRUE, pattern = "\\.R$")
  skip_if(length(rfiles) == 0, "no R source files found (not running against source tree)")
  rlines <- as.character(unlist(lapply(rfiles, readLines)))
  rwords <- unlist(strsplit(rlines, "\\s+"))
  checks <- grep("CHECKS$", rwords, fixed = TRUE, value = TRUE)
  expect_identical(anyDuplicated(checks), 0L)
})
