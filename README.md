goodpractice
================

> Advice on R Package Building

[![Linux Build
Status](https://travis-ci.org/MangoTheCat/goodpractice.svg?branch=master)](https://travis-ci.org/MangoTheCat/goodpractice)
[![Windows Build
status](https://ci.appveyor.com/api/projects/status/github/MangoTheCat/goodpractice?svg=true)](https://ci.appveyor.com/project/gaborcsardi/goodpractice)
[![](http://www.r-pkg.org/badges/version/goodpractice)](http://www.r-pkg.org/pkg/goodpractice)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/goodpractice)](http://www.r-pkg.org/pkg/goodpractice)
[![Coverage
Status](https://img.shields.io/codecov/c/github/MangoTheCat/goodpractice/master.svg)](https://codecov.io/github/MangoTheCat/goodpractice?branch=master)

Give advice about good practices when building R packages. Advice
includes functions and syntax to avoid, package structure, code
complexity, code formatting, etc.

## Installation

``` r
source("https://install-github.me/MangoTheCat/goodpractice")
# or
# install.packages("devtools")
devtools::install_github("mangothecat/goodpractice") 
```

## Usage

``` r
library(goodpractice)
gp("<my-package>")
```

## Example

``` r
library(goodpractice)
# use example package contained in the goodpractice package
pkg_path <- system.file("bad1", package = "goodpractice")
g <- gp(pkg_path)
g
```

    #> ── GP badpackage ──────────────────────────────────────────────────────────
    #> 
    #> It is good practice to
    #> 
    #>   ✖ not use "Depends" in DESCRIPTION, as it can cause name
    #>     clashes, and poor interaction with other packages. Use
    #>     "Imports" instead.
    #>   ✖ omit "Date" in DESCRIPTION. It is not required and it gets
    #>     invalid quite often. A build date will be added to the package
    #>     when you perform `R CMD build` on it.
    #>   ✖ add a "URL" field to DESCRIPTION. It helps users find
    #>     information about your package online. If your package does
    #>     not have a homepage, add an URL to GitHub, or the CRAN package
    #>     package page.
    #>   ✖ add a "BugReports" field to DESCRIPTION, and point it to a bug
    #>     tracker. Many online code hosting services provide bug
    #>     trackers for free, https://github.com, https://gitlab.com,
    #>     etc.
    #>   ✖ omit trailing semicolons from code lines. They are not needed
    #>     and most R coding standards forbid them
    #> 
    #>     R/semicolons.R:4:30
    #>     R/semicolons.R:5:29
    #>     R/semicolons.R:9:38
    #> 
    #>   ✖ not import packages as a whole, as this can cause name clashes
    #>     between the imported packages. Instead, import only the
    #>     specific functions you need.
    #>   ✖ fix this R CMD check ERROR: VignetteBuilder package not
    #>     declared: ‘knitr’ See section ‘The DESCRIPTION file’ in the
    #>     ‘Writing R Extensions’ manual.
    #>   ✖ avoid 'T' and 'F', as they are just variables which are set to
    #>     the logicals 'TRUE' and 'FALSE' by default, but are not
    #>     reserved words and hence can be overwritten by the user.
    #>     Hence, one should always use 'TRUE' and 'FALSE' for the
    #>     logicals.
    #> 
    #>     R/tf.R:NA:NA
    #>     R/tf.R:NA:NA
    #>     R/tf.R:NA:NA
    #>     R/tf.R:NA:NA
    #>     R/tf.R:NA:NA
    #>     ... and 4 more lines
    #> 
    #> ───────────────────────────────────────────────────────────────────────────

``` r
# show all available checks
# all_checks()

# run only a specific check
g_url <- gp(pkg_path, checks = "description_url")
g_url
```

    #> ── GP badpackage ──────────────────────────────────────────────────────────
    #> 
    #> It is good practice to
    #> 
    #>   ✖ add a "URL" field to DESCRIPTION. It helps users find
    #>     information about your package online. If your package does
    #>     not have a homepage, add an URL to GitHub, or the CRAN package
    #>     package page.
    #> ───────────────────────────────────────────────────────────────────────────

``` r
# which checks were carried out?
checks(g_url)
```

    #> [1] "description_url"

``` r
# which checks failed?
failed_checks(g)
```

    #> [1] "no_description_depends"                
    #> [2] "no_description_date"                   
    #> [3] "description_url"                       
    #> [4] "description_bugreports"                
    #> [5] "lintr_trailing_semicolon_linter"       
    #> [6] "no_import_package_as_a_whole"          
    #> [7] "rcmdcheck_package_dependencies_present"
    #> [8] "truefalse_not_tf"

``` r
# show the first 5 checks carried out and their results
results(g)[1:5,]
```

    #>                    check result
    #> 1                   covr     NA
    #> 2              cyclocomp   TRUE
    #> 3 no_description_depends  FALSE
    #> 4    no_description_date  FALSE
    #> 5        description_url  FALSE

## License

MIT © Mango Solutions
