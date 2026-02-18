# goodpractice 1.0.5.9000 (dev version)

* `gp()` now fails if the path provided to it is not a package (does not contain a
  DESCRIPTION file) (#190, @maelle)
* goodpractice now uses cli, and no longer depends on crayon and clisymbols (@olivroy, #167).
* If your editor supports it, goodpractice now prints clickable hyperlinks to console.
- New `describe_check()` function to print descriptions of all implemented checks (@152)
* New `r_file_extension` check: flags R scripts using `.r` or `.q` instead of `.R` (#121).

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
