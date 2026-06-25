---
name: goodpractice4agents
description: Use the goodpractice R package to identify package issues, then repair them. Trigger whenever an agent is asked to fix, clean up, reduce, or pass goodpractice, lintr, or R CMD check failures in an R package, or to bring a package up to good-practice standards.
compatibility: Requires the goodpractice R package installed, with read and write access within an R package repository.
---

# Fix issues identified by 'goodpractice' package

**AGENT** These are instructions for you to modify the R package code in the
current directory. You will be using the `goodpractice` tool which flags
several categories (`groups`) of common issues in R package development. Each
"issue" is called a failure, and your job is to modify the code to ensure as
few failures as possible.

Only ever load the package you are checking with `devtools::load_all()`, never
`R CMD install`. `load_all()` reflects your in-progress edits immediately
without a build step, so each re-check sees your latest changes; an installed
copy would mask them until rebuilt.

## Important Note

It may not be possible to fix all failures, and judgement will sometimes be
necessary. This is particularly likely for failures in the `lintr` group. It
may be impossible to get `lintr` checks to run with no failures. You should
nevertheless try hard. For example, line length linters often fail for extended
URLs. If they are part of normal code, they may be broken by using `paste()` or
similar, but if they are embedded in documentation within a `\url{}`, they will
need to be left as is, and consequently fail. Similar kinds of subtleties will
apply to many other linters.

If a linter appears to be giving a false failure, you may append a terminal `#
nolint` to the end of the line. Avoid wrapping blocks between `# nolint start`
and `# nolint end`: a terminal `# nolint` suppresses exactly one line you have
judged, whereas a start/end block silently mutes every linter across a region,
which is how genuine issues get hidden.

## Questions for user

**AGENT**: You are to ask the user the following two questions first, and then
follow these instructions in sequence.

The questions you must pose are in italics here, anything after the italicised
text provides additional context for you, and not for the user. Ask each
question individually, and request a simple "y/n" response for each before
proceeding.

1. _Do you want to include "tidyverse" checks (y/n)?_
2. _Do you want me to report results of each check group and wait? (y/n)?_

If the user answers no to question 2, additionally ask:

2a. _Do you want separate commits for each check group (y), or a single commit
with all changes (n)?_

## Tasks for agent

1. Load the `goodpractice` package and identify check groups by running this R
   code:
    ``` r
    library(goodpractice)
    groups <- all_check_groups()
    ```
    Then exclude `c("covr", "cyclocomp", "rcmdcheck", "revdep")` from `groups`
    If the user wants to exclude "tidyverse" checks, also exclude those from
    `groups`.
2. **Stop and tell user** which check groups are going to be run, and ask
   whether they'd like to exclude any of those groups. Exclude any indicated
   ones from `groups` as well.
3. Unless the user says otherwise, keep groups in default order, but with
   `lintr` as the last group. If the user wants to run `tidyverse` checks,
   append that as the very last check group.

**The following checks are to be run for each `group` in `groups`:**

4. Run `g <- gp(checks = checks_by_group(group))`
5. Export checks to a temp JSON file: `file <- tempfile(fileext = ".json"); export_json(g, file)`.
6. Read all failures as `failures <- jsonlite::read_json(file)$failures` (AGENT:
   use your own tools to read this JSON file). The name of each item in
   `failures` is called `nm` from here on. As you progress through the groups,
   keep tallies of numbers of failures in each group, and the total across all
   groups.

**The following checks are to be run for each `failure` in `failures` (with name `nm`):**

7. Get description of failure and recommended fix with `describe_check(nm)`
8. For each `instance` of that failure type, extract `filename` and `line`.
    8a. If `filename` is in a location ignored by `.Rbuildignore`, ignore that
        failure and proceed to next `instance`.
    8b. If `filename` is in `man/` directory and package uses `roxygen2`
        (indicated in `DESCRIPTION` file), then do not edit `man/` files
        directly, but edit at source in `R/`, and run `roxygen2::roxygenise()`
        to update corresponding `man/` entry.
    8c. If it's a `roxygen2`-group check for `@inheritParams` instead of
        duplicated parameters, only automatically do that if parameter
        descriptions are identical. Use the `get_dup_params.R` code in "Useful
        functions" below to extract those. If they seem very close, use your
        judgement, or ask user if you are unsure.
9. Go to that location and fix the `failure`, then back to 7. for next `failure`

Once you've finished all `failures` in one group:

10. If `group` is one of "lintr", or "package_structure", run
    `devtools::load_all(); testthat::test_package()` to make sure nothing
    breaks. For any test failures, make sure you first understand why your
    changes caused those, and only update test code if it was obviously
    designed to respond to previous conditions that no longer apply.
11. If `group` is `roxygen2`, then run `roxygen2::roxygenise()` to update
    corresponding `man/` entries.
12. Rerun `gp(checks = checks_by_group(group))`, and extract number of
    remaining failures from the resultant JSON file. Provide a summary to user,
    noting initial and final number of failing checks. If the user asked you to
    stop after each `group`, do that, but ensure you remind them to keep going
    once they seem satisfied.
13. Loop back to next `group` in `groups`

Once you've finished all of the initial groups - that is, those minus the
excluded groups:

14. Repeat steps 4 - 13 for `gp(checks=checks_by_group(g))` for each of the
    three remaining groups, `g = c("covr", "cyclocomp", "rcmdcheck")`. Execute
    all steps for each single group. (`revdep` was excluded in step 1 and is
    deliberately not re-run here: it checks reverse dependencies, which is slow,
    network-dependent, and outside the scope of fixing this package's own code.)

### Final steps

15. Remove any terminal blank spaces at any line ends, and terminal blank lines
    in files.
16. Summarise all updates for the user.

---

## Useful functions

**get_dup_params.R**

``` r
library(goodpractice)
ns <- getNamespace("goodpractice")
g <- gp(checks = "roxygen2_duplicate_params")
blocks <- g[["roxygen2"]][["blocks"]]
extract_block_params <- get("extract_block_params", envir = ns)

all_params <- unlist(lapply(blocks, extract_block_params), recursive = FALSE)
keys <- vapply(all_params, function(p) paste(p[["name"]], p[["description"]], sep = "|||"), character(1))
files <- vapply(all_params, function(p) p[["file"]], character(1))
lines_v <- vapply(all_params, function(p) as.character(p[["line"]]), character(1))
duped_keys <- unique(keys[duplicated(keys)])

for (key in duped_keys) {
    idxs <- which(keys == key)
    dup_files <- unique(basename(files[idxs]))
    if (length(dup_files) < 2) next
    pname <- sub("[|][|][|].*", "", key)
    pdesc <- sub(".*[|][|][|]", "", key)
    cat(sprintf("PARAM: %s\n  DESC: %s\n", pname, substr(pdesc, 1, 120)))
    for (i in idxs) {
        cat(sprintf("  -> %s line %s\n", basename(files[i]), lines_v[i]))
    }
    cat("\n")
}
```
