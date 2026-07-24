---
name: Bug report
about: Create a report to help us improve
title: "[BUG] <description of bug>"
labels: bug

---

**Steps to follow in reporting a bug with `goodpractice`**

If you encounter a bug when testing your package, please include fully reproducible code using [the `reprex`](https://reprex.tidyverse.org) and [`gert` packages](https://docs.ropensci.org/gert).

For your repo at `https://github.com/<org>/<repo>`, start with the following code:

``` r
# <your reprex code here>

u <- "https://github.com/<org>/<repo>"
path <- gert::git_clone(u, file.path(tempdir(), basename(u)))
g <- goodpractice::gp(path)

# Please avoid using default full printed output. If the bug
# is only about one aspect of the checks, please use the
# extra "groups" argument of print. For example,
print(g, "namespace")
print(g, "rd")

packageVersion("goodpractice")
R.Version()$version.string
#sessionInfo()
```

Thanks! :smile:
