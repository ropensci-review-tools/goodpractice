# goodpractice 1.0.5.9000 (dev version)

* New optional tidyverse style guide checks: 21 lintr-based checks plus 2
  structural checks (R file naming, test file mirroring).
  Opt in via `checks = c(default_checks(), tidyverse_checks())`.
* New `default_checks()` and `tidyverse_checks()` helper functions.
  `gp()` now defaults to `default_checks()` instead of `all_checks()`,
  keeping optional check sets out of the default run.
* New `has_readme` and `has_news` checks for package documentation
  completeness (#45).* `gp()` now fails if the path provided to it is not a package (does not contain a
  DESCRIPTION file) (#190, @maelle)
* goodpractice now uses cli, and no longer depends on crayon and clisymbols (@olivroy, #167).
* If your editor supports it, goodpractice now prints clickable hyperlinks to console.
- New `describe_check()` function to print descriptions of all implemented checks (@152)
* New `r_file_extension` check: flags R scripts using `.r` or `.q` instead of `.R` (#121).
* New `print_return_invisible` check: flags print methods that don't return `invisible(x)` (#49).
* New `vignette_no_rm_list` check: flags `rm(list = ls())` in vignettes (#20).
* New `vignette_no_setwd` check: flags `setwd()` in vignettes (#21).
* R code inspection now uses treesitter for AST-based parsing instead of
  regex or `getParseData()`. Checks like `print_return_invisible`,
  `tidyverse_no_missing`, and `tidyverse_export_order` benefit from more
  robust and faster code analysis.
* New `reverse_dependencies` check: queries CRAN for reverse dependencies and advises
  running `revdepcheck::revdep_check()` before submission.
* `gp_advice()` gains a `type` parameter (`"error"`, `"info"`, `"warning"`) to
  control the output symbol and colour. Checks can return
  `list(status = TRUE, type = "info")` to display informational messages
  without a failure cross.
* Prep step error handling refactored into `run_prep_step()` helper. New prep
  functions can use `run_prep_step(state, "name", function() { ... }, quiet)`
  instead of manually wrapping work in `try()` and emitting warnings on failure.
* New `spelling` check: flags misspelled words in documentation via `spelling::spell_check_package()` (#84).
* New DESCRIPTION checks (#122, #85):
  - `description_not_start_with_package`: Description should not start with
    "This package"
  - `description_urls_in_angle_brackets`: URLs in Description must be wrapped
    in angle brackets
  - `description_doi_format`: DOIs should use `<doi:...>` not full URLs
  - `description_urls_not_http`: URLs should use https not http
  - `no_description_duplicate_deps`: No duplicate packages across dependency
    fields
  - `description_valid_roles`: Authors@R roles must be valid MARC relator codes
  - `description_pkgname_single_quoted`: Package names in Title/Description
    must be single-quoted

# goodpractice 1.0.5

* New maintainer: rOpenSci
* Package reinstated on CRAN, after archiving of previous version.
* CRAN fixes - skipping failing test and adding \alias{goodpractice} to package Rd
* Adding docs.ropensci site to DESCRIPTION

# goodpractice 1.0.3

Additions:

* Limit for cyclomatic complexity check can be adjusted using the `goodpractice.cyclocomp.limit` option, default 50 (#132, @fabian-s).
* The number of lines printed to the console by each check result can be set using the new `positions_limit` parameter into `print()` - previously it was always 5 lines (#130, @fabian-s).
* GitHub Actions now used for CI/CD checks (#145), as well as to calculate code coverage with {covr} and build the package site with {pkgdown}.



Bugfixes:

* Documentation for custom checks significantly improved (#133, @fabian-s).
* Year updated in `LICENSE`, and `LICENSE.md` added to clarify that {goodpractice} uses the MIT license (#144).




# goodpractice 1.0.2 (2018-06-14)

First CRAN release.

- added 2 vignettes
- added examples
- added tests
- added pkgdown site
- fixed check on library/require calls on windows
- wrapped prep steps in try




# goodpractice 1.0.0

First public release.
