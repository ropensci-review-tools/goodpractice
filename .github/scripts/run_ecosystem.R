label <- Sys.getenv("GP_LABEL", "unknown")
pkg_dirs <- list.dirs("test-repos", recursive = FALSE)

tidyverse_pkgs <- strsplit(
  Sys.getenv("TIDYVERSE_PACKAGES", ""), ","
)[[1]]

options(goodpractice.exclude_check_groups = c(
  "covr", "rcmdcheck", "cyclocomp"
))

results <- lapply(pkg_dirs, function(pkg) {
  pkg_name <- basename(pkg)
  checks <- if (pkg_name %in% tidyverse_pkgs) {
    goodpractice::all_checks()
  } else {
    goodpractice::default_checks()
  }
  cat("Running", pkg_name, "...\n")
  tryCatch({
    g <- suppressWarnings(
      goodpractice::gp(pkg, checks = checks, quiet = TRUE)
    )
    res <- goodpractice::results(g)
    data.frame(
      package = pkg_name,
      check = res$check,
      passed = res$passed,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
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
