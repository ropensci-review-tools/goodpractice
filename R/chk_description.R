
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
