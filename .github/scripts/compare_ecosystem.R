main <- read.csv("results/main.csv", stringsAsFactors = FALSE)
pr <- read.csv("results/pr.csv", stringsAsFactors = FALSE)

key <- c("package", "check")
merged <- merge(main, pr, by = key, suffixes = c("_main", "_pr"),
                all = TRUE)

new_failures <- merged[
  (is.na(merged$passed_main) | merged$passed_main) &
  !is.na(merged$passed_pr) & !merged$passed_pr,
]

new_passes <- merged[
  !is.na(merged$passed_main) & !merged$passed_main &
  (is.na(merged$passed_pr) | merged$passed_pr),
]

new_checks <- merged[is.na(merged$passed_main) & !is.na(merged$passed_pr), ]
removed_checks <- merged[!is.na(merged$passed_main) & is.na(merged$passed_pr), ]

lines <- character()
lines <- c(lines, "## Ecosystem check results\n")

pkgs <- unique(merged$package)
lines <- c(lines, sprintf(
  "Ran goodpractice on **%d packages** comparing main vs this PR.\n",
  length(pkgs)
))

if (nrow(new_failures) > 0) {
  lines <- c(lines, sprintf(
    "### New failures (%d)\n", nrow(new_failures)
  ))
  lines <- c(lines, "| Package | Check |")
  lines <- c(lines, "|---------|-------|")
  for (i in seq_len(nrow(new_failures))) {
    lines <- c(lines, sprintf(
      "| %s | %s |",
      new_failures$package[i], new_failures$check[i]
    ))
  }
  lines <- c(lines, "")
} else {
  lines <- c(lines, "### No new failures\n")
}

if (nrow(new_passes) > 0) {
  lines <- c(lines, sprintf(
    "### New passes (%d)\n", nrow(new_passes)
  ))
  lines <- c(lines, "| Package | Check |")
  lines <- c(lines, "|---------|-------|")
  for (i in seq_len(min(nrow(new_passes), 20))) {
    lines <- c(lines, sprintf(
      "| %s | %s |",
      new_passes$package[i], new_passes$check[i]
    ))
  }
  if (nrow(new_passes) > 20) {
    lines <- c(lines, sprintf(
      "| ... | ... and %d more |", nrow(new_passes) - 20
    ))
  }
  lines <- c(lines, "")
}

if (nrow(new_checks) > 0) {
  lines <- c(lines, sprintf(
    "### New checks added (%d)\n",
    length(unique(new_checks$check))
  ))
}

summary_main <- sum(main$passed, na.rm = TRUE)
summary_pr <- sum(pr$passed, na.rm = TRUE)
lines <- c(lines, sprintf(
  "**Total passing**: %d (main) -> %d (PR)",
  summary_main, summary_pr
))

writeLines(lines, "ecosystem_comparison.md")
cat("Comparison written to ecosystem_comparison.md\n")
