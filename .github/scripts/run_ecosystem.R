label <- Sys.getenv("GP_LABEL", "unknown")
pkg_dirs <- list.dirs("test-repos", recursive = FALSE)

results <- lapply(pkg_dirs, function(pkg) {
  pkg_name <- basename(pkg)
  tryCatch({
    g <- goodpractice::gp(
      pkg,
      checks = goodpractice::default_checks(),
      quiet = TRUE
    )
    res <- goodpractice::results(g)
    data.frame(
      package = pkg_name,
      check = res$check,
      passed = res$passed,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      package = pkg_name,
      check = "ERROR",
      passed = NA,
      stringsAsFactors = FALSE
    )
  })
})

df <- do.call(rbind, results)
dir.create("results", showWarnings = FALSE)
outfile <- file.path("results", paste0(label, ".csv"))
write.csv(df, outfile, row.names = FALSE)
cat("Wrote", nrow(df), "results to", outfile, "\n")
