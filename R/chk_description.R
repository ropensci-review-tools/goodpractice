
#' @include lists.R

CHECKS$no_description_depends <- make_check(

  description = 'No "Depends" in DESCRIPTION',
  tags = c("info", "DESCRIPTION"),
  preps = "description",

  gp = 'not use "Depends" in DESCRIPTION, as it can cause name
        clashes, and poor interaction with other packages. Use "Imports"
        instead.',

  check = function(state) {
    if(inherits(state$description, "try-error")) return(NA)
    
    deps <- state$description$get_deps()
    ## Remove 'methods' and R, these are OK
    deps <- deps[! deps$package %in% c("methods", "R"), , drop = FALSE]
    ! 'Depends' %in% deps$type
  }
)

## --------------------------------------------------------------------

CHECKS$no_description_date <- make_check(

  description = 'No "Date" in DESCRIPTION',
  tags = c("info", "DESCRIPTION"),
  preps = "description",

  gp = 'omit "Date" in DESCRIPTION. It is not required and it
        gets invalid quite often. A build date will be added to
        the package when you perform `R CMD build` on it.',

  check = function(state) {
    if(inherits(state$description, "try-error")) return(NA)
    
    ! state$description$has_fields('Date')
  }
)

## --------------------------------------------------------------------

CHECKS$description_url <- make_check(

  description = 'URL in DESCRIPTION',
  tags = c("info", 'DESCRIPTION'),
  preps = 'description',

  gp = 'add a "URL" field to DESCRIPTION. It helps users find information
        about your package online. If your package does not have a homepage,
        add an URL to GitHub, or the CRAN package package page.',

  check = function(state) {
    if(inherits(state$description, "try-error")) return(NA)
    
    state$description$has_fields("URL")
  }
)

## --------------------------------------------------------------------

CHECKS$description_not_start_with_package <- make_check(

  description = 'Description does not start with package name reference',
  tags = c("info", "DESCRIPTION"),
  preps = "description",

  gp = 'not start the Description field by referring to the package
        itself, e.g. "This package ...", "This is a package ...",
        or "The FooBar package ...". Describe what the package does
        directly, e.g. "Provides tools for ..." instead of
        "This package provides tools for ...".',

  check = function(state) {
    if(inherits(state$description, "try-error")) return(NA)

    desc_text <- state$description$get_field("Description")
    pkg_name <- state$description$get_field("Package")
    starts_this_pkg <- grepl("^This\\s+(is\\s+a\\s+)?package\\b", desc_text, ignore.case = TRUE)
    starts_the_pkg <- grepl(paste0("^The\\s+", pkg_name, "\\s+package\\b"), desc_text, ignore.case = TRUE)
    !(starts_this_pkg || starts_the_pkg)
  }
)

## --------------------------------------------------------------------

CHECKS$description_urls_in_angle_brackets <- make_check(

  description = "URLs in Description are enclosed in angle brackets",
  tags = c("info", "DESCRIPTION"),
  preps = "description",

  gp = 'enclose URLs in the Description field in angle brackets,
        e.g. <https://example.com>. CRAN requires angle brackets
        around URLs for auto-linking.',

  check = function(state) {
    if(inherits(state$description, "try-error")) return(NA)

    desc_text <- state$description$get_field("Description")
    bare_url <- "(?<!<)(https?://[^\\s,)>]+)"
    !grepl(bare_url, desc_text, perl = TRUE)
  }
)

## --------------------------------------------------------------------

CHECKS$description_doi_format <- make_check(

  description = "DOIs in Description use <doi:...> format",
  tags = c("info", "DESCRIPTION"),
  preps = "description",

  gp = 'use the <doi:10.prefix/suffix> format for DOIs in the
        Description field instead of a full URL like
        <https://doi.org/10.prefix/suffix>. CRAN requires
        the <doi:...> notation for auto-linking.',

  check = function(state) {
    if(inherits(state$description, "try-error")) return(NA)

    desc_text <- state$description$get_field("Description")
    !grepl("https?://doi\\.org/", desc_text)
  }
)

## --------------------------------------------------------------------

CHECKS$description_urls_not_http <- make_check(

  description = "URLs in Description use https not http",
  tags = c("info", "DESCRIPTION"),
  preps = "description",

  gp = 'consider using https:// instead of http:// for URLs in the
        Description field. CRAN prefers secure URLs where available.
        Check if there might be a secure https:// alternative.',

  check = function(state) {
    if(inherits(state$description, "try-error")) return(NA)

    desc_text <- state$description$get_field("Description")
    !grepl("http://", desc_text, fixed = TRUE)
  }
)

## --------------------------------------------------------------------

CHECKS$no_description_duplicate_deps <- make_check(

  description = "No duplicate dependencies in DESCRIPTION",
  tags = c("info", "DESCRIPTION"),
  preps = "description",

  gp = 'not list the same package in multiple dependency fields
        (e.g. both Imports and Suggests). Each dependency should
        appear only once.',

  check = function(state) {
    if(inherits(state$description, "try-error")) return(NA)

    deps <- state$description$get_deps()
    deps <- deps[deps$package != "R", , drop = FALSE]
    !anyDuplicated(deps$package)
  }
)

## --------------------------------------------------------------------

CHECKS$description_valid_roles <- make_check(

  description = "Authors have valid roles in DESCRIPTION",
  tags = c("info", "DESCRIPTION"),
  preps = "description",

  gp = 'use only standard R author roles in Authors@R. Valid roles are:
        aut (author), com (compiler), cph (copyright holder),
        cre (maintainer), ctb (contributor), ctr (contractor),
        dtc (data contributor), fnd (funder), rev (reviewer),
        ths (thesis advisor), trl (translator).',

  check = function(state) {
    if(inherits(state$description, "try-error")) return(NA)

    warned <- FALSE
    authors <- tryCatch(
      withCallingHandlers(
        state$description$get_authors(),
        warning = function(w) {
          if (grepl("Invalid role", conditionMessage(w))) {
            warned <<- TRUE
          }
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) NULL
    )
    if (is.null(authors)) return(NA)
    !warned
  }
)

## --------------------------------------------------------------------

CHECKS$description_pkgname_single_quoted <- make_check(

  description = "Package names in Title/Description are single-quoted",
  tags = c("info", "DESCRIPTION"),
  preps = "description",

  gp = 'single-quote package names in the Title and Description fields,
        e.g. \'dplyr\' not dplyr. CRAN requires that software names
        are single-quoted.',

  check = function(state) {
    if(inherits(state$description, "try-error")) return(NA)

    deps <- state$description$get_deps()
    pkg_names <- deps$package[deps$package != "R"]
    if (length(pkg_names) == 0) return(TRUE)

    title <- state$description$get_field("Title")
    desc_text <- state$description$get_field("Description")
    text <- paste(title, desc_text)

    for (pkg in pkg_names) {
      unquoted <- paste0("(?<!')\\b", pkg, "\\b(?!')")
      if (grepl(unquoted, text, perl = TRUE)) return(FALSE)
    }
    TRUE
  }
)

## --------------------------------------------------------------------

CHECKS$description_bugreports <- make_check(

  description = "BugReports in DESCRIPTION",
  tags = c("info", 'DESCRIPTION'),
  preps = 'description',

  gp = 'add a "BugReports" field to DESCRIPTION, and point it to a bug
        tracker. Many online code hosting services provide bug trackers
        for free, https://github.com, https://gitlab.com, etc.',

  check = function(state) {
    if(inherits(state$description, "try-error")) return(NA)
    
    state$description$has_fields('BugReports')
  }
)

## --------------------------------------------------------------------
