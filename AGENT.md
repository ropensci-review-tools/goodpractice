# goodpractice — AI coding instructions

## Package purpose

goodpractice checks R packages for common issues and best practices. It runs external tools (R CMD check, lintr, covr, etc.), caches their output, then evaluates a configurable set of checks against that cached state.

## Architecture

Three-layer plugin system:

1. **Preps** — run external tools, store results in `state`
2. **Checks** — evaluate rules against `state`, return pass/fail with optional positions
3. **Reporting** — format results for console, RStudio markers, JSON export

### State object

A list that accumulates prep results and check outcomes as it flows through `gp()`:

```r
state <- list(
  path = ".",
  package = "mypkg",
  rcmdcheck = <prep result>,
  lintr = <prep result>,
  ...
  checks = list(
    check_name = <check result>,
    ...
  )
)
```

### Global registries

`R/lists.R` defines two global lists populated at load time by side effects:

```r
PREPS <- list()
CHECKS <- list()
```

Every `prep_*.R` file appends to `PREPS`, every `chk_*.R` file appends to `CHECKS`. Collate order in DESCRIPTION ensures `lists.R` loads first.

## Adding a new prep

Create `R/prep_<name>.R`. The file must start with `#' @include lists.R` for collate ordering.

```r
#' @include lists.R

PREPS$<name> <- function(state, path = state$path, quiet) {
  state$<name> <- try(<compute_result>(path), silent = quiet)
  if (inherits(state$<name>, "try-error")) {
    warning("Prep step for <name> failed.")
  }
  state
}
```

Key rules:
- Wrap the computation in `try(..., silent = quiet)` so failures don't crash the run
- Never use `return()` inside `try()` — it escapes the enclosing function. Extract a helper function if you need early returns
- Always return `state`
- Store results under `state$<name>` matching the prep name

## Adding new checks

Create `R/chk_<name>.R`. The file must start with `#' @include lists.R`.

```r
#' @include lists.R

CHECKS$<check_name> <- make_check(
  description = "Short present-tense description",
  tags = c("category", "subcategory"),
  preps = "<prep_name>",
  gp = "Advice text shown on failure.",
  check = function(state) {
    if (inherits(state$<prep_name>, "try-error")) {
      return(list(status = NA, positions = list()))
    }
    # ... evaluate rule ...
    list(status = <logical>, positions = <list of position objects>)
  }
)
```

### Check return values

Either a bare logical (`TRUE`/`FALSE`/`NA`) or a list:

```r
list(
  status = TRUE,
  positions = list()
)
```

### Position objects

```r
list(
  filename = "R/file.R",
  line_number = 42L,
  column_number = NA_integer_,
  ranges = list(),
  line = "the_offending_code_line"
)
```

Filenames are relative to the package root. `line` is a short string shown to the user identifying the problem.

### Factory functions

When a prep produces structured data and multiple checks extract from it the same way, create a factory:

```r
make_<name>_check <- function(description, gp, <field_or_filter>, tags = NULL) {
  make_check(
    description = description,
    tags = c("category", tags),
    preps = "<prep_name>",
    gp = gp,
    check = function(state) {
      # shared extraction logic using <field_or_filter>
    }
  )
}
```

See `make_rcmd_check()` in `R/chk_rcmdcheck.R` for the canonical example.

### Handling prep failures in checks

Always guard against prep failure at the top of every check function:

```r
if (inherits(state$<prep_name>, "try-error")) {
  return(list(status = NA, positions = list()))
}
```

Returning `NA` status means the check is skipped, not failed.

### Tags

First tag indicates severity and maps to RStudio marker type: `"error"`, `"warning"`, `"info"`, `"style"`, `"usage"`. Additional tags categorise the check: `"documentation"`, `"DESCRIPTION"`, `"NAMESPACE"`, etc.

### Naming conventions

- Prep names match the tool/concept: `rcmdcheck`, `lintr`, `rd`, `roxygen2`
- Check names: `<source>_<detail>` — e.g. `lintr_assignment_linter`, `rd_has_examples`
- `no_` prefix for checks that flag things that shouldn't exist: `no_description_depends`
- File names: `R/prep_<name>.R`, `R/chk_<name>.R`

## DESCRIPTION updates

When adding new preps or checks:
- Add any new package dependencies to `Imports` (not `Suggests`)
- Run `roxygen2::roxygenise()` to update the `Collate` field — the `@include lists.R` directive ensures correct load order
- Add `@importFrom` directives for external functions

## Testing

Tests live in `tests/testthat/` and use testthat edition 3.

### Test fixtures

Minimal R packages under `tests/testthat/`:
- `good/` — well-formed package that passes all checks
- `bad1/`, `bad2/`, `bad3/`, `baddoc/` — packages with specific issues
- Name new fixtures descriptively: `bad_rd/`, `bad_roxygen/`, `no_roxygen/`, etc.

Each fixture needs at minimum: `DESCRIPTION`, `NAMESPACE`, `R/` with at least one file.

### Test structure

```r
test_that("check_name fails when <condition>", {
  gp_res <- gp("bad_fixture", checks = "check_name")
  res <- results(gp_res)
  expect_false(res$result[res$check == "check_name"])
})

test_that("check_name passes when <condition>", {
  gp_res <- gp("good", checks = "check_name")
  res <- results(gp_res)
  expect_true(res$result[res$check == "check_name"])
})
```

For checks with positions, also verify the position output:

```r
pos <- failed_positions(gp_res)$check_name
lines <- vapply(pos, `[[`, "", "line")
expect_true(any(grepl("expected_string", lines)))
```

### Snapshot tests

`_snaps/describe-check.md` captures `describe_check()` output for all registered checks. Update snapshots with `testthat::snapshot_accept()` after adding new checks.

## Customization API

Users can extend goodpractice at runtime without modifying package source:

```r
my_prep <- make_prep("mydata", function(path, quiet) { ... })
my_check <- make_check(
  description = "...", preps = "mydata", gp = "...",
  check = function(state) { ... }
)
gp(".",
   extra_preps = list(mydata = my_prep),
   extra_checks = list(my_check = my_check))
```

## Key files

| File | Purpose |
|---|---|
| `R/lists.R` | Global `PREPS` and `CHECKS` registries, `all_checks()`, `describe_check()` |
| `R/gp.R` | Main `gp()` entry point — orchestrates prep and check execution |
| `R/customization.R` | `make_prep()`, `make_check()`, `prepare_preps()`, `prepare_checks()` |
| `R/api.R` | `results()`, `checks()`, `failed_checks()`, `failed_positions()`, `export_json()` |
| `R/print.R` | `print.goodPractice()` — console output with positions |
| `R/rstudio_markers.R` | RStudio source marker integration |
| `R/utils.R` | Helpers: `get_package_name()`, `%||%`, `r_package_files()`, etc. |
