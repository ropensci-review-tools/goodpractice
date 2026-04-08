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
  exclude_path = character(),
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

### Check groups

Every check belongs to at least one group (matching its prep name). Users discover groups via `all_check_groups()` and select checks by group via `checks_by_group()`. Groups can be excluded via `goodpractice.exclude_check_groups` option or `GP_EXCLUDE_CHECK_GROUPS` envvar.

## Adding a new prep

Create `R/prep_<name>.R`. The file must start with `#' @include lists.R` for collate ordering. Use `run_prep_step()` for consistent error handling:

```r
#' @include lists.R prep_utils.R

PREPS$<name> <- function(state, path = state$path, quiet) {
  run_prep_step(state, "<name>", function(path) {
    <compute_result>(path)
  }, path = path, silent = quiet)
}
```

Key rules:
- Use `run_prep_step()` — it wraps in `try()`, stores result in `state$<name>`, and emits a `cli::cli_warn()` on failure
- Never use `return()` inside `try()` — extract a helper function if you need early returns
- If `state$exclude_path` matters, pass it through to file-listing functions
- Always return `state`

## Adding new checks

Create `R/chk_<name>.R`. The file must start with `#' @include lists.R`.

```r
#' @include lists.R

CHECKS$<check_name> <- make_check(
  description = "Short present-tense description",
  tags = c("category", "subcategory"),
  preps = "<prep_name>",
  gp = "Advice text with {.code cli} {.fn markup}.",
  check = function(state) {
    if (inherits(state$<prep_name>, "try-error"))
      return(na_result())
    # ... evaluate rule ...
    check_result(<logical>, <list of check_position() objects>)
  }
)
```

### cli markup in gp strings

All `gp` message strings support cli inline markup. Use:
- `{.fn func}` for function names
- `{.code expression}` for code
- `{.file path}` for file paths
- `{.field name}` for DESCRIPTION fields
- `{.pkg name}` for package names
- `{.url url}` for URLs

Literal braces must be escaped as `{{` and `}}`.

### Check return values

Always use `check_result()` (defined in `R/utils.R`) to construct return values:

```r
check_result(TRUE)                        # passed, no positions
check_result(FALSE, problems)             # failed with positions
check_result(!grepl(...))                 # dynamic status, no positions
check_result(TRUE, type = "info", ...)    # extra fields via ...
```

`na_result()` is shorthand for `check_result()` (status = NA, positions = empty).

Do **not** construct `list(status = ..., positions = ...)` manually.

### Position objects

Always use `check_position()` (defined in `R/utils.R`) to construct positions:

```r
check_position("R/file.R", 42L, line = "the_offending_line")
```

Signature: `check_position(filename, line_number, column_number, ranges, line)`.
Defaults: `line_number = NA_integer_`, `column_number = NA_integer_`,
`ranges = list()`, `line = ""`.

Filenames are relative to the package root. `line` is a short string shown to the user identifying the problem.

Do **not** construct position lists manually.

### Factory functions

When a prep produces structured data and multiple checks extract from it the same way, create a factory:

```r
make_<name>_check <- function(description, gp, ..., tags = NULL) {
  make_check(
    description = description,
    tags = c("category", tags),
    preps = "<prep_name>",
    gp = gp,
    check = function(state) { ... }
  )
}
```

Examples: `make_rcmd_check()`, `make_lintr_check()`, `make_rd_check()`, `make_urlchecker_check()`.

### Handling prep failures in checks

Always guard against prep failure at the top of every check function:

```r
if (inherits(state$<prep_name>, "try-error"))
  return(na_result())
```

Returning `NA` status means the check is skipped, not failed.

### Tags

First tag indicates severity: `"error"`, `"warning"`, `"info"`, `"style"`. Additional tags categorise the check: `"documentation"`, `"DESCRIPTION"`, `"NAMESPACE"`, `"lintr"`, `"testing"`, etc.

### Naming conventions

- Prep names match the tool/concept: `rcmdcheck`, `lintr`, `rd`, `roxygen2`
- Check names: `<source>_<detail>` — e.g. `lintr_assignment_linter`, `rd_has_examples`
- `no_` prefix for checks that flag things that shouldn't exist: `no_description_depends`
- File names: `R/prep_<name>.R`, `R/chk_<name>.R`

### Internal functions

All internal (non-exported) functions must have `#' @noRd` above them. Functions without roxygen2 blocks still need a bare `#' @noRd` tag.

## Signalling conditions

Use cli throughout — never use `message()`, `warning()`, or `stop()`:
- `cli::cli_abort()` instead of `stop()`
- `cli::cli_warn()` instead of `warning()`
- `cli::cli_inform()` instead of `message()`

## Configuration options

| Option | Envvar | Default | Purpose |
|--------|--------|---------|---------|
| `goodpractice.exclude_check_groups` | `GP_EXCLUDE_CHECK_GROUPS` | `NULL` | Skip entire check groups |
| `goodpractice.exclude_path` | `GP_EXCLUDE_PATH` | `NULL` | Skip specific files from checks |
| `goodpractice.cyclocomp_limit` | — | `15` | Max cyclomatic complexity |
| `goodpractice.function_length_limit` | — | `50` | Max function body lines |

## DESCRIPTION updates

When adding new preps or checks:
- Add any new package dependencies to `Imports` (not `Suggests`)
- Run `devtools::document()` to update the `Collate` field and NAMESPACE
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
  expect_false(res$passed[res$check == "check_name"])
})

test_that("check_name passes when <condition>", {
  gp_res <- gp("good", checks = "check_name")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "check_name"])
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
  description = "...", preps = "mydata",
  gp = "advice with {.code cli} markup.",
  check = function(state) { ... }
)
gp(".",
   extra_preps = list(mydata = my_prep),
   extra_checks = list(my_check = my_check))
```

## Key files

| File | Purpose |
|---|---|
| `R/lists.R` | Global `PREPS`/`CHECKS` registries, `all_checks()`, `all_check_groups()`, `checks_by_group()` |
| `R/gp.R` | `gp()` entry point, `validate_pkg_path()`, `run_preps()`, `run_checks()`, exclusion logic |
| `R/customization.R` | `make_prep()`, `make_check()` |
| `R/api.R` | `results()`, `checks()`, `failed_checks()`, `failed_positions()`, `export_json()` |
| `R/print.R` | `print.goodPractice()` — cli-based console output with `cli_bullets()` |
| `R/prep_utils.R` | `run_prep_step()` — shared try/warn boilerplate for preps |
| `R/utils.R` | `na_result()`, `filter_excluded_paths()`, `safe_parse()`, etc. |
| `R/treesitter.R` | `ts_parse()`, `ts_get()` — treesitter-based R code analysis |
| `R/rstudio_markers.R` | RStudio source marker integration |
