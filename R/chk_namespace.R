
#' @include lists.R

CHECKS$no_import_package_as_a_whole <- make_check(

  description = 'Packages are not imported as a whole',
  tags = c("warning", "NAMESPACE"),
  preps = "namespace",

  gp = 'not import packages as a whole, as this can cause name
        clashes between the imported packages. Instead, import
        only the specific functions you need.',

  check = function(state) {
    if(inherits(state$namespace, "try-error")) return(NA)
    imports <- state$namespace$imports
    all(vapply(imports, length, 1L) > 1)
  }
)

CHECKS$no_export_pattern <- make_check(

  description = "exportPattern in NAMESPACE file",
  tags = c("warning", "NAMESPACE"),
  preps = "namespace",

  gp = 'not use exportPattern in NAMESPACE. It can lead to
        exporting functions unintendedly. Instead, export
        functions that constitute the external API of your
        package.',

  check = function(state) {
    if(inherits(state$namespace, "try-error")) return(NA)
    length(state$namespace$exportPatterns) == 0
  }
)
