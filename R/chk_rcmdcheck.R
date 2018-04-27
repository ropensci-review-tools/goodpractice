
## Last updated: 2016-03-16
## To be peroidically updated, according to changes in check.R:
## https://github.com/wch/r-source/commits/trunk/src/library/tools/R/check.R

#' Wrapper on make_check, specific to R CMD check
#'
#' @param description A description of the check.
#' @param type Type of notification, one of "warnings", "notes" or "errors".
#' @param pattern The text pattern identifying the check.
#' @param tags Tags to be passed on to make_check.
#' @param preps Preps to be passed on to make_check.
#' @param ... Currently not supported.
#'
#' @keywords internal
#' @include lists.R

make_rcmd_check <- function(
  description, pattern, gp = NULL, type = c("warnings", "notes", "errors"),
  tags = NULL, preps = NULL, ...) {

  type <- match.arg(type)
  kw <- c(warnings = "WARNING", notes = "NOTE", errors = "ERROR")
  chk_pat <- paste0("^.* \\.\\.\\. ", kw[type])

  etag <- c(warnings = "warning", notes = "info", errors = "error")[type]

  make_check(
    description = description,
    tags = c(etag, "rcmdcheck", tags),
    preps = c("rcmdcheck", preps),
    gp = function(state) {
      paste(
        sub(
          chk_pat, paste0("fix this R CMD check ", kw[type], ":"),
          grep(pattern, state$rcmdcheck[[type]], value = TRUE)
        ),
        collapse = "\n"
      )
    },
    check = function(state) {
      if(inherits(state$rcmdcheck, "try-error")) return(NA)
      ! any(grep(pattern, state$rcmdcheck[[type]]))
    }
  )
}

CHECKS$rcmdcheck_package_directory_exists <- make_rcmd_check(

  "Package directory exists",
  type = "errors",
  pattern = "Package directory .* does not exist"
)

CHECKS$rcmdcheck_vignette_leftovers <- make_rcmd_check(

  "No vignette leftovers",
  pattern = "for left-overs from vignette generation"
)

CHECKS$rcmdcheck_qpdf_present_for_checking <- make_rcmd_check(

  "'qpdf' must be installed for checks",
  type = "warnings",
  pattern = "'qpdf' is needed for checks on size reduction of PDFs"
)

CHECKS$rcmdcheck_vignettes_at_right_place <- make_rcmd_check(

  "Vignettes are in 'vignettes'",
  type = "notes",
  pattern = "Vignette sources only in 'inst/doc':"
)

CHECKS$rcmdcheck_portable_file_names <- make_rcmd_check(

  "File names are portable",
  type = "errors",
  pattern = "Please rename the files and try again"
)

CHECKS$rcmdcheck_duplicate_file_names <- make_rcmd_check(

  "No file names that only differ in case",
  type = "errors",
  pattern = "Found the following files with duplicate lower-cased file names"
)

CHECKS$rcmdcheck_ascii_file_names <- make_rcmd_check(

  "File names are portable (ASCII)",
  type = "warnings",
  pattern = "These are not fully portable file names"
)

CHECKS$rcmdcheck_portable_paths <- make_rcmd_check(

  "Paths are short enough to be portable",
  type = "errors",
  pattern = "Found the following non-portable file path"
)

CHECKS$rcmdcheck_proper_permissions <- make_rcmd_check(

  "Files have correct permissions",
  type = "errors",
  pattern = "Found the following files with insufficient permissions"
)

CHECKS$rcmdcheck_executable_files <- make_rcmd_check(

  "configure and cleanup files must be executable",
  type = "warnings",
  pattern = "The following files should most likely be executable"
)

CHECKS$rcmdcheck_description_meta <- make_rcmd_check(

  "DESCRIPTION meta information is correct",
  type = "error",
  pattern = "DESCRIPTION meta-information .* ERROR"
)

CHECKS$rcmdcheck_malformed_title_or_description <- make_rcmd_check(

  "Title and Description fields are valid",
  type = "notes",
  pattern = "Malformed (Title|Description)"
)

CHECKS$rcmdcheck_portable_description_encoding <- make_rcmd_check(

  "Portable DESCRIPTION Encoding",
  type = "warnings",
  pattern = "Encoding '.*' is not portable"
)

CHECKS$rcmdcheck_description_encoding_known <- make_rcmd_check(

  "Encoding of DESCRIPTION is defined",
  type = "warnings",
  pattern = "Unknown encoding with non-ASCII data"
)

CHECKS$rcmdcheck_description_ascii_tags <- make_rcmd_check(

  "All tags in DESCRIPTION are ASCII",
  type = "warnings",
  pattern = "All field tags must be ASCII"
)

CHECKS$rcmdcheck_description_ascii_fields <- make_rcmd_check(

  "All tags in DESCRIPTION are ASCII, or Encoding is defined",
  type = "warnings",
  pattern = "Fields with non-ASCII values"
)

CHECKS$rcmdcheck_standardizable_license <- make_rcmd_check(

  "Package license is standardizable",
  type = "warnings",
  pattern = "Standardizable: FALSE"
)

CHECKS$rcmdcheck_license_file_pointers <- make_rcmd_check(

  "License file pointers are correct",
  type = "warnings",
  pattern = "Invalid license file pointers"
)

CHECKS$rcmdcheck_non_standard_license <- make_rcmd_check(

  "Non-standard, but standardizable license",
  type = "notes",
  pattern = "Standardized license specification"
)

CHECKS$rcmdcheck_deprecated_license <- make_rcmd_check(

  "Deprecated license",
  type = "notes",
  pattern = "Deprecated license"
)

CHECKS$rcmdcheck_not_permitted_license_restrictions <- make_rcmd_check(

  "Not permitted license restrictions",
  type = "notes",
  pattern = "License components with restrictions not permitted"
)

CHECKS$rcmdcheck_template_licenses <- make_rcmd_check(

  "Template licenses have a LICENSE file",
  type = "notes",
  pattern = "License components which are templates and need"
)

CHECKS$rcmdcheck_authors_at_r_field <- make_rcmd_check(

  "Valid Authors@R field",
  type = "notes",
  pattern = "Malformed Authors@R field"
)

CHECKS$rcmdcheck_valid_author_in_authors_at_r <- make_rcmd_check(

  "Can extract Author from Authors@R",
  type = "notes",
  pattern = "Cannot extract Author field from Authors@R field:"
)

CHECKS$rcmdcheck_author_in_authors_at_r <- make_rcmd_check(

  "Author is in Authors@R",
  type = "notes",
  pattern = "Authors@R field gives no person with author role."
)

CHECKS$rcmdcheck_authors_at_r_valid_roles <- make_rcmd_check(

  "Authors@R has valid roles for all authors",
  type = "notes",
  pattern = "Authors@R field gives persons with no valid roles:"
)

CHECKS$rcmdcheck_maintainer_in_authors_at_r <- make_rcmd_check(

  "Authors@R has package Maintainer",
  type = "notes",
  pattern = "Cannot extract Maintainer field from Authors@R field"
)

CHECKS$rcmdcheck_single_maintainer_in_authors_at_r <- make_rcmd_check(

  "Authors@R has one package Maintainer only",
  type = "notes",
  pattern = "Authors@R field gives more than one person with maintainer role"
)

CHECKS$rcmdcheck_valid_maintainer_in_authors_at_t <- make_rcmd_check(

  "Authors@R has a valid Maintainer role person",
  type = "notes",
  pattern = "Authors@R field gives no person with maintainer role"
)

CHECKS$rcmdcheck_stale_author <- make_rcmd_check(

  "No stale Author field if Authors@R is present",
  type = "notes",
  pattern = "Author field differs from that derived from Authors@R"
)

CHECKS$rcmdcheck_stale_maintainer <- make_rcmd_check(

  "No state Maintainer field if Authors@R is present",
  type = "notes",
  pattern = "Maintainer field differs from that derived from Authors@R"
)

CHECKS$rcmdcheck_needscompilation_field_valid <- make_rcmd_check(

  "NeedsCompilation field must be 'yes' or 'no'",
  type = "notes",
  pattern = "NeedsCompilation field must take value 'yes' or 'no'"
)

CHECKS$rcmdcheck_multiple_dependency_modes <- make_rcmd_check(

  "Packages should be listed as one type of dependency (except for LinkingTo)",
  type = "notes",
  pattern = paste0(
    "Packages? listed in more than one of ",
    "Depends, Imports, Suggests, Enhances"
  )
)

CHECKS$rcmdcheck_unused_linkingto <- make_rcmd_check(

  "LinkingTo field makes sense, there are headers to include",
  type = "notes",
  pattern = "are unused as (it|they) (has|have) no 'include' directory"
)

CHECKS$rcmdcheck_versioned_linkingto <- make_rcmd_check(

  "Versioned 'LinkingTo' needs R >= 3.0.2",
  type = "notes",
  pattern = "Versioned 'LinkingTo' values for"
)

CHECKS$rcmdcheck_linkingto_without_compiled_code <- make_rcmd_check(

  "'LinkingTo' field only makes sense if package has compiled code",
  type = "notes",
  pattern = "'LinkingTo' field is unused: package has no 'src' directory"
)

CHECKS$rcmdcheck_broken_vignettes_1 <- make_rcmd_check(

  "Vignette output not in build/vignette.rds",
  type = "warnings",
  pattern = "Vignette(s) without any output listed in 'build/vignette.rds'"
)

CHECKS$rcmdcheck_broken_vignettes_2 <- make_rcmd_check(

  "All outputs from build/vignette.rds are in the package",
  type = "warnings",
  pattern = "Output(s) listed in 'build/vignette.rds' but not in package:"
)

CHECKS$rcmdcheck_defunct_top_level <- make_rcmd_check(

  "Some top level files are defunct: install.R, R_PROFILE.R",
  type = "warnings",
  pattern = "These files are defunct."
)

CHECKS$rcmdcheck_valid_readme_and_news <- make_rcmd_check(

  "README.md and NEWS.md must work with pandoc",
  type = "warnings",
  pattern = "Conversion of '.*(README|NEWS)\\.md.*' failed"
)

CHECKS$rcmdcheck_stale_license_file <- make_rcmd_check(

  "LICENSE (and LICENCE) must be mentioned in DESCRIPTION",
  type = "notes",
  pattern = "not mentioned in the DESCRIPTION file"
)

CHECKS$rcmdcheck_stale_inst_license_file <- make_rcmd_check(

  "inst/LICENSE (and inst/LICENCE) must be mentioned in DESCRIPTION",
  type = "notes",
  pattern = paste0(
    "will install at top-level and (is|are) not mentioned ",
    "in the DESCRIPTION file"
  )
)

CHECKS$rcmdcheck_non_standard_top_level_files <- make_rcmd_check(

  "Top-level files are standardized",
  type = "notes",
  pattern = "Non-standard files?/director(ies|y) found at top level"
)

CHECKS$rcmdcheck_copyright_file_in_inst <- make_rcmd_check(

  "Copyright file should be in inst/",
  type = "notes",
  pattern = "Copyright information should be in file inst/COPYRIGHTS"
)

CHECKS$rcmdcheck_authors_in_inst <- make_rcmd_check(

  "AUTHORS file should be in inst/",
  type = "notes",
  pattern = "Authors information should be in file inst/AUTHORS"
)

CHECKS$rcmdcheck_leftover_files <- make_rcmd_check(

  "Check for left-over files",
  type = "notes",
  pattern = "The following files look like leftovers"
)

CHECKS$rcmdcheck_empty_index_file <- make_rcmd_check(

  "INDEX file must not be empty if present",
  type = "warnings",
  pattern = "Empty file 'INDEX'"
)

CHECKS$rcmdcheck_empty_demo_index_file <- make_rcmd_check(

  "demo/00Index must not be empty if present",
  type = "warnings",
  pattern = "Empty or missing file demo.*00Index"
)

CHECKS$rcmdcheck_proper_demo_index_file <- make_rcmd_check(

  "demo/00Index file is valid if it exists",
  type = "warnings",
  pattern = "cannot read index information in file"
)

CHECKS$rcmdcheck_demos_are_indexed <- make_rcmd_check(

  "All demos are in demo/00Index",
  type = "warnings",
  pattern = "Demos with missing or empty index information"
)

CHECKS$rcmdcheck_stale_demos_in_index <- make_rcmd_check(

  "Demos in the index exist",
  type = "warnings",
  pattern = "Demo index entries without corresponding demo:"
)

CHECKS$rcmdcheck_vignette_index <- make_rcmd_check(

  "Vignettes have index entries",
  type = "warnings",
  pattern = "Vignettes with missing or empty"
)

CHECKS$rcmdcheck_has_r_source_files <- make_rcmd_check(

  paste0(
    "Package must have R source files (or R/sysdata.rda) if R ",
    "directory is present"
  ),
  type = "warnings",
  pattern = "Found directory 'R' with no source files"
)

CHECKS$rcmdcheck_r_directory_uppercase <- make_rcmd_check(

  "R directory must be upper case",
  type = "warnings",
  pattern = "Most likely, this should be 'R'"
)

CHECKS$rcmdcheck_man_directory_lowercase <- make_rcmd_check(

  "man directory must be lower case",
  type = "warnings",
  pattern = "Most likely, this should be 'man'"
)

CHECKS$rcmdcheck_data_directory_lowercase <- make_rcmd_check(

  "data directory must be lower case",
  type = "warnings",
  pattern = "Most likely, this should be 'data'"
)

CHECKS$rcmdcheck_rcheck_directories <- make_rcmd_check(

  "No leftover .Rcheck directories",
  type = "warnings",
  pattern = paste0(
    "(Found the following directory with the name of a check directory",
    "|Found the following directories with names of check directories)")
)

CHECKS$rcmdcheck_leftover_dvi_pdf_build_directories <- make_rcmd_check(

  "No leftover Rd2dvi or Rd2pdf directories",
  type = "warnings",
  pattern = paste0(
    "Found the following director(y|ies) with the name ",
    "of a Rd2pdf director"
  )
)

CHECKS$rcmdcheck_leftover_vs_directories <- make_rcmd_check(

  "No leftover .git, .svn (etc.) version control directories",
  type = "warnings",
  pattern = paste0(
    "Found the following director(y|ies) with the name of a ",
    "version control director"
  )
)

CHECKS$rcmdcheck_invalid_file_names <- make_rcmd_check(

  "File names in some directories are restricted",
  type = "warnings",
  pattern = "Subdirectory '.*' contains invalid file names"
)

CHECKS$rcmdcheck_empty_data_directory <- make_rcmd_check(

  "data directory must not be empty, if it exists",
  type = "warnings",
  pattern = "Subdirectory 'data' contains no data sets"
)

CHECKS$rcmdcheck_empty_demo_directory <- make_rcmd_check(

  "demo directory must not be empty, if it exists",
  type = "warnings",
  pattern = "Subdirectory 'demo' contains no demos."
)

CHECKS$rcmdcheck_demos_are_ascii <- make_rcmd_check(

  "Demos should use ASCII characters",
  type = "warnings",
  pattern = "Demos with non-ASCII characters:"
)

CHECKS$rcmdcheck_demos_have_valid_code <- make_rcmd_check(

  "Demos should contain valid R code",
  type = "warnings",
  pattern = "Demos which do not contain valid R code"
)

CHECKS$rcmdcheck_empty_exec_directory <- make_rcmd_check(

  "exec directory must not be empty, if it exists",
  type = "warnings",
  pattern = "Subdirectory 'exec' contains no files."
)

CHECKS$rcmdcheck_empty_inst_directory <- make_rcmd_check(

  "inst directory must not be empty, if it exists",
  type = "warnings",
  pattern = "Subdirectory 'inst' contains no files."
)

CHECKS$rcmdcheck_src_without_sources <- make_rcmd_check(

  "src directory must contain some source files, if it exists",
  type = "warnings",
  pattern = "Subdirectory 'src' contains no source files."
)

CHECKS$rcmdcheck_inst_interference <- make_rcmd_check(

  "Some directory names in inst are not allowed",
  type = "warnings",
  pattern = "with package subdirectories used by R"
)

CHECKS$rcmdcheck_news_rd_is_valid <- make_rcmd_check(

  "inst/NEWS.Rd is valid if present",
  type = "warnings",
  pattern = "Problems with news in 'inst/NEWS.Rd':"
)

CHECKS$rcmdcheck_citation_file_is_valid <- make_rcmd_check(

  "inst/CITATION is valid if present",
  type = "warnings",
  pattern = "Invalid citation information in 'inst/CITATION'"
)

CHECKS$rcmdcheck_citation_file_at_standard_place <- make_rcmd_check(

  "CITATION is in inst/, not somewhere else",
  type = "notes",
  pattern = "Found the following CITATION file in a non-standard place"
)

CHECKS$rcmdcheck_r_files_are_ascii <- make_rcmd_check(

  "R files should be ASCII",
  type = "warnings",
  pattern = "Found the following files? with non-ASCII characters"
)

CHECKS$rcmdcheck_errors_in_r_files <- make_rcmd_check(

  "R files do not have syntax errors",
  type = "errors",
  pattern = "R files for syntax errors .*Error in file"
)

CHECKS$rcmdcheck_warnings_in_r_files <- make_rcmd_check(

  "R files do not throw warnings",
  type = "warnings",
  pattern = "R files for syntax errors .*Warnings? in file"
)

CHECKS$rcmdcheck_undeclared_imports <- make_rcmd_check(

  "'::' and ':::' imports must be declared",
  type = "warnings",
  pattern = "'::' or ':::' imports? not declared from:"
)

CHECKS$rcmdcheck_undeclared_library_require <- make_rcmd_check(

  "'library' and 'require' calls must be declared",
  type = "warnings",
  pattern = "'library' or 'require' calls? not declared from:"
)

CHECKS$rcmdcheck_undeclared_loadnamespace_requirenamespace <- make_rcmd_check(

  "'loadNamespace' and 'requireNamespace' calls must be declared",
  type = "warnings",
  pattern = "'loadNamespace' or 'requireNamespace' calls? not declared from:"
)

CHECKS$rcmdcheck_library_require_to_attached <- make_rcmd_check(

  "'library' and 'require' calls to attached packages are not needed",
  type = "notes",
  pattern = paste0(
    "('library' or 'require' calls to packages already attached by Depends:",
    "|'library' or 'require' call to .* which was already attached by Depends)")
)

CHECKS$rcmdcheck_library_require_in_package_code <- make_rcmd_check(

  paste("Do not use 'library' or 'require' in package code. Use '::' or",
        "'requireNamespace' instead"),
  type = "notes",
  pattern = paste0(
    "'library' or 'require' calls in package code:",
    "'library' or 'require' call to .* in package code.")
)

CHECKS$rcmdcheck_imports_not_imported_from <- make_rcmd_check(

  "Packages declared in 'Imports' must be imported from",
  type = "notes",
  pattern = "Namespaces? in Imports field not imported from:"
)

CHECKS$rcmdcheck_depends_not_imported_from <- make_rcmd_check(

  "Packages in 'Depends' must be imported from",
  type = "notes",
  pattern = "Packages? in Depends field not imported from:"
)

CHECKS$rcmdcheck_missing_or_unexported_objects <- make_rcmd_check(

  "Exported objects must be present",
  type = "notes",
  pattern = "Missing or unexported object"
)

CHECKS$rcmdcheck_unneeded_triple_colon <- make_rcmd_check(

  "Use '::' to call exported functions, not ':::'",
  type = "notes",
  pattern = "':::' calls? which should be '::':"
)

CHECKS$rcmdcheck_triple_colon_imported_objects_exist <- make_rcmd_check(

  "All objects imported via ':::' must exist",
  type = "notes",
  pattern = "Missing objects? imported by ':::' calls:"
)

CHECKS$rcmdcheck_unexported_base_objects_imported <- make_rcmd_check(

  "':::' imports to unexported objects, from base packages",
  type = "warnings",
  pattern = "Unexported objects? imported by ':::' calls"
)

CHECKS$rcmdcheck_unexported_objects_imported <- make_rcmd_check(

  "':::' imports to unexported objects",
  type = "notes",
  pattern = "Unexported objects? imported by ':::' calls"
)

CHECKS$rcmdcheck_triple_colon_to_itself <- make_rcmd_check(

  "':::' imports to the package itself are usually not needed",
  type = "notes",
  pattern = "There are ::: calls to the package's namespace in its code."
)

CHECKS$rcmdcheck_triple_colon_to_unknown <- make_rcmd_check(

  "':::' imports to unavailable packages",
  type = "notes",
  pattern = "Unavailable namespaces? imported from by ':::' calls:"
)

CHECKS$rcmdcheck_data_call_to_undeclared_package <- make_rcmd_check(

  "Packages used in 'data(package=)' must be declared",
  type = "notes",
  pattern = "data(package=)' calls? not declared from"
)

CHECKS$rcmdcheck_exported_s3_methods_are_registered <- make_rcmd_check(

  "Exported S3 methods are registered",
  type = "warnings",
  pattern = paste0(
    "Found the following apparent S3 methods ",
    "exported but not registered"
  )
)

CHECKS$rcmdcheck_replacement_function_arg_names <- make_rcmd_check(

  "Replacement functions must have a 'value' argument",
  type = "warnings",
  pattern = "The argument of a replacement function"
)

CHECKS$rcmdcheck_foreign_calls_have_package_argument <- make_rcmd_check(

  "Foreign calls must have a 'PACKAGE' argument",
  type = "warnings",
  pattern = "Foreign function calls? without 'PACKAGE' argument:"
)

CHECKS$rcmdcheck_foreign_call_empty_package_argument <- make_rcmd_check(

  "'PACKAGE' argument of foreign calls must not be empty",
  type = "warnings",
  pattern = "Foreign function calls? with empty 'PACKAGE' argument:"
)

CHECKS$rcmdcheck_foreign_call_to_base_package <- make_rcmd_check(

  "Foreign calls should not call base packages",
  type = "warnings",
  pattern = "Foreign function calls? to a base package"
)

CHECKS$rcmdcheck_foreign_calls_to_another_package <- make_rcmd_check(

  "Should not call another package with foreign calls",
  type = "notes",
  pattern = "Foreign function calls? to a different package"
)

CHECKS$rcmdcheck_foreign_calls_to_unknown_package <- make_rcmd_check(

  "Packages called from foreign calls must be declared",
  type = "warnings",
  pattern = "Undeclared packages? in foreign function calls"
)

CHECKS$rcmdcheck_foreign_registration_problems <- make_rcmd_check(

  "Check for registration problems of foreign function calls",
  type = "notes",
  pattern = "Registration problem"
)

CHECKS$rcmdcheck_foreign_calls_with_dup <- make_rcmd_check(

  "The 'DUP' argument of foreign calls is ignored",
  type = "notes",
  pattern = "Calls? with DUP"
)

CHECKS$rcmdcheck_loading_and_unloading <- make_rcmd_check(

  "Check loading and unloading the package",
  type = "errors",
  pattern = "Incorrect [(]un[)]loading of package"
)

CHECKS$rcmdcheck_first_lib_obsolete <- make_rcmd_check(

  ".First.lib is obsolete",
  type = "notes",
  pattern = ".First.lib is obsolete and will not be used in R >= 3.0.0"
)

CHECKS$rcmdcheck_startup_function_arguments <- make_rcmd_check(

  "Package startup function arguments should be lib* and pkg*",
  type = "notes",
  pattern = paste0(
    "Package startup functions should have two ",
    "arguments with names starting with"
  )
)

CHECKS$rcmdcheck_startup_function_change_search_path <- make_rcmd_check(

  paste("Package startup functions should not change the search path.",
        "I.e. do not call 'library' or 'require'"),
  type = "notes",
  pattern = "Package startup functions should not change the search path"
)

CHECKS$rcmdcheck_startup_function_messages <- make_rcmd_check(

  paste0(
    "Package startup functions should use 'packageStartupMessage' ",
    "to generate messages"
  ),
  type = "notes",
  pattern = paste0(
    "Package startup functions should use .packageStartupMessage. ",
    "to generate messages"
  )
)

CHECKS$rcmdcheck_statup_function_unsafe_calls <- make_rcmd_check(

  "Package startup functions should not call installed.packages",
  type = "notes",
  pattern = "Package startup functions should not call"
)

CHECKS$rcmdcheck_last_lib_needs_to_be_exported <- make_rcmd_check(

  ".Last.lib will not be used unless it is exported",
  type = "notes",
  pattern = ".Last.lib will not be used unless it is exported"
)

CHECKS$rcmdcheck_detach_function_arguments <- make_rcmd_check(

  paste0(
    "Package detach functions should have one argument with ",
    "name starting with 'lib'"
  ),
  type = "notes",
  pattern = paste0(
    "Package detach functions should have one argument ",
    "with name starting with"
  )
)

CHECKS$rcmdcheck_detach_no_library_dynam_unload <- make_rcmd_check(

  "Package detach functions should not call 'library.dynam.unload'",
  type = "notes",
  pattern = "Package detach functions should not call"
)

CHECKS$rcmdcheck_unsafe_calls <- make_rcmd_check(

  "Check for possibly unsafe calls: unlockBinding, assignInNamespace",
  type = "notes",
  pattern = "Found the following possibly unsafe calls"
)

CHECKS$rcmdcheck_partial_argument_match <- make_rcmd_check(

  "Partial argument matches are fragile",
  type = "notes",
  pattern = "partial argument match of"
)

CHECKS$rcmdcheck_undefined_globals <- make_rcmd_check(

  "Check for undefined globals",
  type = "notes",
  pattern = "Undefined global functions or variables"
)

CHECKS$rcmdcheck_avoid_internal_calls <- make_rcmd_check(

  "No .Internal calls are allowed",
  type = "notes",
  pattern = paste0(
    "(Found a .Internal call in the following function",
    "|Found .Internal calls in the following functions)")
)

CHECKS$rcmdcheck_avoid_internal_calls_s4 <- make_rcmd_check(

  "No .Internal calls are allowed in S4 methods",
  type = "notes",
  pattern = paste0(
    "(Found a.Internal call in methods for the following S4 generic",
    "|Found .Internal calls in methods for the following S4 generics)")
)

CHECKS$rcmdcheck_avoid_internal_calls_rc <- make_rcmd_check(

  "No .Internal calls are allowed in reference class methods",
  type = "notes",
  pattern = paste0(
    "(Found a .Internal call in methods for the following reference class",
    "|Found .Internal calls in methods for the following reference classes)")
)

CHECKS$rcmdcheck_assignment_to_globalenv <- make_rcmd_check(

  "Assignment to .GlobalEnv is not allowed",
  type = "notes",
  pattern = "Found the following assignments to the global environment"
)

CHECKS$rcmdcheck_avoid_using_attach <- make_rcmd_check(

  "It is better to avoid attach()",
  type = "notes",
  pattern = "Found the following calls to attach"
)

CHECKS$rcmdcheck_data_into_globalenv <- make_rcmd_check(

  "Avoid loading data into the global environment",
  type = "notes",
  pattern = paste0(
    "Found the following calls to data.. loading into the global ",
    "environment"
  )
)

CHECKS$rcmdcheck_obsolete_platform_specific <- make_rcmd_check(

  "Avoid calling obsolete and platform specific functions",
  type = "notes",
  pattern = "Found an obsolete/platform-specific call in the following function"
)

CHECKS$rcmdcheck_obsolete_platform_specific_s4 <- make_rcmd_check(

  "Avoid calling obsolete and platform specific functions from S4 methods",
  type = "notes",
  pattern = paste0(
    "Found an obsolete/platform-specific call in methods ",
    "for the following S4 generic"
  )
)

CHECKS$rcmdcheck_obsolete_platform_specific_rc <- make_rcmd_check(

  paste0(
    "Avoid calling obsolete and platform specific functions ",
    "from reference class methods"
  ),
  type = "notes",
  pattern = paste0(
    "Found an obsolete/platform-specific call in methods ",
    "for the following reference class"
  )
)

CHECKS$rcmdcheck_deprecated_functions <- make_rcmd_check(

  "Avoid calling deprecated functions",
  type = "notes",
  pattern = "Found the deprecated function"
)

CHECKS$rcmdcheck_defunct_removed_functions <- make_rcmd_check(

  "Avoid calling defunct and removed functions",
  type = "warnings",
  pattern = "Found the defunct/removed function"
)

CHECKS$rcmdcheck_avoid_platform_specific_devices <- make_rcmd_check(

  "Avoid using platform specific devices",
  type = "notes",
  pattern = "Found the platform-specific device"
)

CHECKS$rcmdcheck_rd_empty_sections <- make_rcmd_check(

  "Check Rd files for empty sections",
  type = "warnings",
  pattern = "Dropping empty section"
)

CHECKS$rcmdcheck_rd_problems <- make_rcmd_check(

  "Check Rd files for problems",
  type = "notes",
  pattern = "Rd files"
)

CHECKS$rcmdcheck_rd_duplicated_name <- make_rcmd_check(

  "Check for Rd files with duplicated \\name",
  type = "notes",
  pattern = "Rd files with duplicated name"
)

CHECKS$rcmdcheck_rd_duplicated_alias <- make_rcmd_check(

  "Check for Rd files with duplicated \\alias",
  type = "notes",
  pattern = "Rd files with duplicated alias"
)

CHECKS$rcmdcheck_rd_long_code_lines <- make_rcmd_check(

  "Check for long code lines in Rd files",
  type = "notes",
  pattern = "These lines will be truncated in the PDF manual"
)

CHECKS$rcmdcheck_rd_cross_references <- make_rcmd_check(

  "Check Rd links",
  type = "warnings",
  pattern = "Missing link or links in documentation object"
)

CHECKS$rcmdcheck_missing_docs <- make_rcmd_check(

  "Check for undocumented exported objects",
  type = "warnings",
  pattern = "All user-level objects"
)

CHECKS$rcmdcheck_code_docs_mismatch <- make_rcmd_check(

  "Check for code/documentation mismatches",
  type = "warnings",
  pattern = "for code/documentation mismatches"
)

CHECKS$rcmdcheck_rd_usage <- make_rcmd_check(

  "Check Rd \\usage sections",
  type = "warnings",
  pattern = "Rd \\usage sections"
)

CHECKS$rcmdcheck_rd_s3_usage <- make_rcmd_check(

  "S3 methods should use the \\method markup, not their full name",
  type = "notes",
  pattern = "entries for S3 methods should use.*markup and not their full name."
)

CHECKS$rcmdcheck_rd_contents <- make_rcmd_check(

  "Check Rd contents",
  type = "warnings",
  pattern = "Rd contents"
)

CHECKS$rcmdcheck_unstated_dependencies_in_examples <- make_rcmd_check(

  "Check for unstated dependencies in examples",
  type = "warnings",
  pattern = "for unstated dependencies in examples"
)

CHECKS$rcmdcheck_data_contents <- make_rcmd_check(

  "Check files in 'data' directory, if exists",
  type = "warnings",
  pattern = "contents of 'data' directory"
)

CHECKS$rcmdcheck_undeclared_non_ascii_characters_in_data <- make_rcmd_check(

  "Check for undeclared non-ASCII characters in data",
  type = "warnings",
  pattern = "Warning: found non-ASCII string"
)

CHECKS$rcmdcheck_non_ascii_characters_in_data <- make_rcmd_check(

  "Check for non-ASCII characters in data",
  type = "notes",
  pattern = "data for non-ASCII characters"
)

CHECKS$rcmdcheck_uncompressed_data_files <- make_rcmd_check(

  "Check for data files that could be compressed better",
  type = "warnings",
  pattern = "data for ASCII and uncompressed saves"
)

CHECKS$rcmdcheck_bzip2_xz_requirement <- make_rcmd_check(

  "bzip2 or xz compression needs more recent R version",
  type = "warnings",
  pattern = "Warning: package needs dependence on R [(]>= 2.10[)]"
)

CHECKS$rcmdcheck_uncompressed_sysdata <- make_rcmd_check(

  "Check if sysdata.rda could be compressed better",
  type = "notes",
  pattern = "R/sysdata.rda"
)

CHECKS$rcmdcheck_unneeded_doc_extra <- make_rcmd_check(

  "Check for unneeded extra style files for documentation",
  type = "notes",
  pattern = "The following files are already in R:"
)

CHECKS$rcmdcheck_doc_extra_licenses <- make_rcmd_check(

  paste0(
    "Check if extra doc styles have license that requires ",
    "the distribution of its original sources"
  ),
  type = "notes",
  pattern = "The following files contain a license that requires"
)

CHECKS$rcmdcheck_doc_tex_leftovers <- make_rcmd_check(

  "Check for possible leftover files for docs",
  type = "notes",
  pattern = "The following files look like leftovers/mistakes"
)

CHECKS$rcmdcheck_extra_files_in_docs <- make_rcmd_check(

  "Check for extra files in docs",
  type = "notes",
  pattern = "The following files should probably not be installed"
)

CHECKS$rcmdcheck_extra_directories_in_docs <- make_rcmd_check(

  "Check for extra directories in docs",
  type = "notes",
  pattern = "The following directories should probably not be installed"
)

CHECKS$rcmdcheck_vignettes_in_vignettes <- make_rcmd_check(

  "Vignettes must be vignettes/",
  type = "warnings",
  pattern = paste0(
    "Vignette sources in 'inst/doc' missing ",
    "from the 'vignettes' directory"
  )
)

CHECKS$rcmdcheck_knitr_leftovers <- make_rcmd_check(

  "Check for possible knitr leftover files",
  type = "notes",
  pattern = "The following directories look like leftovers from 'knitr'"
)

CHECKS$rcmdcheck_pdf_file_sizes <- make_rcmd_check(

  "Check if PDF files could be made signicantly smaller",
  type = "notes",
  pattern = "consider running tools::compactPDF.. on these files"
)

CHECKS$rcmdcheck_pdf_file_sizes_gs <- make_rcmd_check(

  "Check if PDF files could be made much smaller with GhostScript and qpdf",
  type = "warnings",
  pattern = paste0(
    "consider running tools::compactPDF.gs_quality = \"ebook\". ",
    "on these files"
  )
)

CHECKS$rcmdcheck_source_line_endings <- make_rcmd_check(

  "Source files have LF (Unix) line endings",
  type = "warnings",
  pattern = "Found the following sources/headers with CR or CRLF line endings"
)

CHECKS$rcmdcheck_makefile_line_endings <- make_rcmd_check(

  "Makefile(s) have LF (Unix) line endings",
  type = "warnings",
  pattern = "Found the following Makefile.s. with CR or CRLF line endings"
)

CHECKS$rcmdcheck_makefile_with_final_lf <- make_rcmd_check(

  "Makefile(s) have a final LF (newline character)",
  type = "notes",
  pattern = "Found the following Makefile.s. without a final LF"
)

CHECKS$rcmdcheck_non_portable_makevars <- make_rcmd_check(

  "Check for non-portable Makevars flags",
  type = "warnings",
  pattern = "Non-portable flags in variable"
)

CHECKS$rcmdcheck_makevars_overriding_user_site <- make_rcmd_check(

  "Check for Makevars flags overriding user/site settings",
  type = "warnings",
  pattern = "Variables overriding user/site settings"
)

CHECKS$rcmdcheck_makevars_and_makevars_in <- make_rcmd_check(

  "Check if package has both Makevars and Makevars.in",
  type = "notes",
  pattern = "Package has both .*Makevars.in.* and .*Makevars"
)

CHECKS$rcmdcheck_gnu_make_required <- make_rcmd_check(

  "Check if GNU make is a system requirement",
  type = "notes",
  pattern = "GNU make is a SystemRequirements"
)

CHECKS$rcmdcheck_gnu_makefile_extensions <- make_rcmd_check(

  "Check if GNU make is needed (and is undeclared)",
  type = "warnings",
  pattern = "Found the following file.s. containing GNU extensions"
)

CHECKS$rcmdcheck_blas_and_lapack_flags <- make_rcmd_check(

  "Check if $(BLAS_LIBS) are used when $(LAPACK_LIBS) are",
  type = "warnings",
  pattern = "apparently using \\$.LAPACK_LIBS. without \\$.BLAS_LIBS. in"
)

CHECKS$rcmdcheck_missing_flibs <- make_rcmd_check(

  "Check if $(FLIBS) is required and missing",
  type = "warnings",
  pattern = "apparently PKG_LIBS is missing \\$.FLIBS. in "
)

CHECKS$rcmdcheck_unsafe_calls_in_compiled_code <- make_rcmd_check(

  "Check for unsafe calls in compiled code",
  type = "notes",
  pattern = "Compiled code should not call entry points which"
)

CHECKS$rcmdcheck_loading_package <- make_rcmd_check(

  "Check loading the package",
  type = "errors",
  pattern = paste0(
    "(Loading this package had a fatal error",
    "|has a loading problem: see the messages)")
)

CHECKS$rcmdcheck_unloading_package <- make_rcmd_check(

  "Check unloading the package",
  type = "warnings",
  pattern = "whether the package can be unloaded cleanly"
)

CHECKS$rcmdcheck_namespace_can_be_loaded <- make_rcmd_check(

  "Check if namespace can be loaded",
  type = "warnings",
  pattern = "whether the namespace can be loaded with stated dependencies"
)

CHECKS$rcmdcheck_namespace_can_be_loaded_safely <- make_rcmd_check(

  "Check if namespace can be loaded safely",
  type = "notes",
  pattern = "whether the namespace can be loaded with stated dependencies"
)

CHECKS$rcmdcheck_namespace_can_be_unloaded <- make_rcmd_check(

  "Check if namespace can be unloaded",
  type = "warnings",
  pattern = "whether the namespace can be unloaded cleanly"
)

CHECKS$rcmdcheck_loading_when_not_on_search_path <- make_rcmd_check(

  "Packages must be able to load when not on the search path",
  type = "warnings",
  pattern = "loading without being on the library search path"
)

CHECKS$rcmdcheck_s3_method_registration <- make_rcmd_check(

  "S3 methods are properly registered",
  type = "warnings",
  pattern = "use of S3 registration"
)

CHECKS$rcmdcheck_encoding_in_ascii_locale <- make_rcmd_check(

  paste0(
    "Check if we are checking a package with non-ascii ",
    "encoding in an ASCII locale"
  ),
  type = "warnings",
  pattern = "checking a package with encoding .* in an ASCII locale"
)

CHECKS$rcmdcheck_examples_run <- make_rcmd_check(

  "Examples must run",
  type = "errors",
  pattern = "Running examples in .* failed"
)

CHECKS$rcmdcheck_examples_run_without_warnings <- make_rcmd_check(

  "Examples shoudl run without warnings",
  type = "warnings",
  pattern = "Found the following significant warnings"
)

CHECKS$rcmdcheck_two_many_cores_used <- make_rcmd_check(

  "Package should not use more than two cores during checks",
  type = "warnings",
  pattern = "Note that CRAN packages must never use more than two"
)

CHECKS$rcmdcheck_can_collect_examples <- make_rcmd_check(

  "Can create a single file with all example",
  type = "errors",
  pattern = "Running massageExamples to create"
)

CHECKS$rcmdcheck_unstated_dependencies_in_tests <- make_rcmd_check(

  "All packages needed for tests are declared",
  type = "warnings",
  pattern = "for unstated dependencies in .*tests"
)

CHECKS$rcmdcheck_tests_pass <- make_rcmd_check(

  "Tests",
  type = "errors",
  pattern = "Last 13 lines of output"
)

CHECKS$rcmdcheck_unstated_dependencies_in_vignettes <- make_rcmd_check(

  "All packages needed for vignettes are declared",
  type = "notes",
  pattern = "for unstated dependencies in vignettes"
)

CHECKS$rcmdcheck_vignette_output_present <- make_rcmd_check(

  "Output from all vignettes are present",
  type = "warnings",
  pattern = "Package vignettes? without corresponding PDF/HTML"
)

CHECKS$rcmdcheck_encoding_defined_in_vignettes <- make_rcmd_check(

  "Non-ASCII vignettes must define their encoding",
  type = "warnings",
  pattern = "Non-ASCII package vignette without specified encoding"
)

CHECKS$rcmdcheck_doc_makefile_uppercase <- make_rcmd_check(

  "inst/doc/Makefile is uppercase",
  type = "warnings",
  pattern = paste0(
    "Found 'inst/doc/makefile': should be 'Makefile' ",
    "and will be ignored"
  )
)

CHECKS$rcmdcheck_calling_r_from_makefile <- make_rcmd_check(

  "R is called properly from Makefile",
  type = "warnings",
  pattern = "Found 'R CMD' in Makefile: should be"
)

CHECKS$rcmdcheck_makefile_line_endings_2 <- make_rcmd_check(

  "Makefile line ending should be LF (Unix)",
  type = "warnings",
  pattern = "Found Makefile with CR or CRLF line endings"
)

CHECKS$rcmdcheck_calling_rscript_from_makefile <- make_rcmd_check(

  "Rscript is called properly from Makefile",
  type = "warnings",
  pattern = "Found 'Rscript' in Makefile: should be"
)

CHECKS$rcmdcheck_correct_vignette_encodings <- make_rcmd_check(

  "Vignettes use the encoding they define",
  type = "warnings",
  pattern = paste0(
    "(Package vignette which is not in its specified encoding",
    "|Package vignettes which are not in their specified encoding)")
)

CHECKS$rcmdcheck_vignettes_load_dependencies <- make_rcmd_check(

  "Vignettes can load dependencies",
  type = "warnings",
  pattern = "Errors in running code in vignettes"
)

CHECKS$rcmdcheck_vignettes_run <- make_rcmd_check(

  "Vignettes run",
  type = "errors",
  pattern = "Errors in running code in vignettes"
)

CHECKS$rcmdcheck_vignettes_build <- make_rcmd_check(

  "Vignettes build",
  type = "warnings",
  pattern = "Warning in re-building vignettes"
)

CHECKS$rcmdcheck_vignettes_build_2 <- make_rcmd_check(

  "Vignettes build",
  type = "notes",
  pattern = "(Warning|Error) in re-building vignettes"
)

CHECKS$rcmdcheck_can_convert_rd_to_pdf <- make_rcmd_check(

  "Can convert Rd manual to PDF",
  type = "errors",
  pattern = "Rd conversion errors"
)

CHECKS$rcmdcheck_can_convert_rd_to_pdf_2 <- make_rcmd_check(

  "Can convert Rd manual to PDF",
  type = "warnings",
  pattern = "LaTeX errors when creating PDF version"
)

CHECKS$rcmdcheck_pdf_without_hyperref <- make_rcmd_check(

  "PDF manual builds without hyperref",
  type = "errors",
  pattern = "PDF version of manual without hyperrefs or index"
)

CHECKS$rcmdcheck_executable_files_in_packages <- make_rcmd_check(

  "Package should not contain executable files",
  type = "warnings",
  pattern = "Found the following executable file"
)

CHECKS$rcmdcheck_hidden_files_and_directories <- make_rcmd_check(

  "Package does not have hidden files and directories",
  type = "notes",
  pattern = "Found the following hidden files and directories"
)

CHECKS$rcmdcheck_installs <- make_rcmd_check(

  "Package installs",
  type = "errors",
  pattern = "Installation failed\\."
)

CHECKS$rcmdcheck_significant_compilation_warnings <- make_rcmd_check(

  "Check for significant warnings when compiling C/C++/Fortran code",
  type = "warnings",
  pattern = "Found the following significant warnings"
)

CHECKS$rcmdcheck_other_compilation_warnings <- make_rcmd_check(

  "Check for significant warnings when compiling C/C++/Fortran code",
  type = "notes",
  pattern = "Found the following warnings"
)

CHECKS$rcmdcheck_reasonable_installed_size <- make_rcmd_check(

  "Package size is reasonable when installed",
  type = "notes",
  pattern = "installed package size"
)

CHECKS$rcmdcheck_description_required_fields <- make_rcmd_check(

  "DESCRIPTION has required fields",
  type = "errors",
  pattern = "Required fields? missing or empty"
)

CHECKS$rcmdcheck_package_name_portable <- make_rcmd_check(

  "Package name is portable",
  type = "warnings",
  pattern = "Package name is not portable"
)

CHECKS$rcmdcheck_description_right_case <- make_rcmd_check(

  "DESCRIPTION is all upper case",
  type = "errors",
  pattern = paste0(
    "File DESCRIPTION does not exist but there ",
    "is a case-insensitive match"
  )
)

CHECKS$rcmdcheck_not_package_bundle <- make_rcmd_check(

  "Package bundles are defunct",
  type = "errors",
  pattern = "is a package bundle -- they are defunct"
)

CHECKS$rcmdcheck_cran_incoming_feasibility_1 <- make_rcmd_check(

  "CRAN incoming feasibility",
  type = "errors",
  pattern = "CRAN incoming feasibility"
)

CHECKS$rcmdcheck_cran_incoming_feasibility_2 <- make_rcmd_check(

  "CRAN incoming feasibility",
  type = "warnings",
  pattern = "CRAN incoming feasibility"
)

CHECKS$rcmdcheck_cran_incoming_feasibility_3 <- make_rcmd_check(

  "CRAN incoming feasibility",
  type = "notes",
  pattern = "CRAN incoming feasibility"
)

CHECKS$rcmdcheck_valid_namespace <- make_rcmd_check(

  "Valid NAMESPACE file",
  type = "errors",
  pattern = "Invalid NAMESPACE file, parsing gives"
)

CHECKS$rcmdcheck_empty_importfrom_in_namespace <- make_rcmd_check(

  "Check for empty importFrom command in NAMESPACE",
  type = "notes",
  pattern = "Namespace with empty importFrom"
)

CHECKS$rcmdcheck_too_many_s3_methods <- make_rcmd_check(

  "R < 3.0.2 had a limit of 500 registered S3 methods",
  type = "notes",
  pattern = "R < 3.0.2 had a limit of 500 registered S3 methods: found"
)

CHECKS$rcmdcheck_package_dependencies_present <- make_rcmd_check(

  "Package dependencies are present for the check",
  type = "errors",
  pattern = "package dependencies"
)

CHECKS$rcmdcheck_if_source_package <- make_rcmd_check(

  "Only *source* packages can be checked",
  type = "errors",
  pattern = "Only \\*source\\* packages can be checked"
)

CHECKS$rcmdcheck_object_files_in_source_package <- make_rcmd_check(

  "Source packages should not contain object files",
  type = "warnings",
  pattern = "Subdirectory .* contains apparent object files/libraries"
)

CHECKS$rcmdcheck_multi_arch_build_dir <- make_rcmd_check(

  "Check for leftover multi-arch build directory",
  type = "warnings",
  pattern = paste0(
    "Found the following directory with a name of ",
    "a multi-arch build directory"
  )
)

CHECKS$rcmdcheck_compilation_leftover_files <- make_rcmd_check(

  "Possible leftover files from a compilation",
  type = "warnings",
  pattern = "These are unlikely file names for src files"
)

CHECKS$rcmdcheck_object_files_in_source_package_2 <- make_rcmd_check(

  "Source packages should not contain object files",
  type = "notes",
  pattern = "Found the following apparent object files/libraries"
)

CHECKS$rcmdcheck_installed_version_included <- make_rcmd_check(

  "Check if an installed version is included by mistake",
  type = "notes",
  pattern = paste0(
    "Subdirectory .* seems to contain an installed ",
    "version of the package"
  )
)

CHECKS$rcmdcheck_some_code_in_docs <- make_rcmd_check(

  "*Some* form of documentation should contain some code to run ",
  type = "warnings",
  pattern = "No examples, no tests, no vignettes"
)
