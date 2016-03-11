
#' @include lists.R

#' Wrapper on make_check, specific to R CMD check
#'
#' @param description
#' @param type
#' @param pattern
#' @param tags
#' @param preps,
#' @param ...
#'
#' @keywords internal

make_rcmd_check <- function(
  description, pattern, gp = NULL, type = c("warnings", "notes", "errors"),
  tags = NULL, preps = NULL, ...) {

  type <- match.arg(type)
  kw <- c(warnings = "WARNING", notes = "NOTE", errors = "ERROR")
  chk_pat <- paste0("^.* \\.\\.\\. ", kw[type])

  make_check(
    description = description,
    tags = c("rcmdcheck", NULL),
    preps = c("rcmdcheck", NULL),
    gp = function(state) {
      paste(
        sub(
          chk_pat, gp %||% "fix this R CMD check error/warning/note:",
          grep(pattern, state$rcmdcheck[[type]], value = TRUE)
        ),
        collapse = "\n"
      )
    },
    check = function(state) {
      ! any(grep(pattern, state$rcmdcheck[[type]]))
    }
  )
}

CHECKS$rcmdcheck_undocumented_code <- make_rcmd_check(

  "Exported code objects are documented",
  pattern = "Undocumented code objects",
  gp = "document all exported code objects (via R CMD check):"
)

CHECKS$rcmdcheck_undocumented_data_sets <- make_rcmd_check(

  "Exported data sets are documented",
  pattern = "Undocumented data sets",
  gp = "document all exported data sets (via R CMD check):"
)

CHECKS$rcmdcheck_undocumented_s4_classes <- make_rcmd_check(

  "Exported S4 classes are documented",
  pattern = "Undocumented S4 classes",
  gp = "document all exported S4 classes (via R CMD check):"
)

CHECKS$rcmdcheck_undocumented_s4_methods <- make_rcmd_check(

  "S4 methods are documented",
  pattern = "Undocumented S4 methods",
  gp = "document all S4 methods of exported S4 classes (via R CMD check):"
)

## TODO: Prototyped non-primitives
## https://github.com/wch/r-source/blob/68c9ff98c09d24aead90f305c2c9bfd69622e005/src/library/tools/R/QC.R#L295

## TODO: Undocumented %s:
## https://github.com/wch/r-source/blob/68c9ff98c09d24aead90f305c2c9bfd69622e005/src/library/tools/R/QC.R#L296

CHECKS$rcmdcheck_functions_in_usages_not_in_code <- make_rcmd_check(

  "Manual page 'usage' refers to code that exists",
  pattern = "Functions or methods with usage in documentation object '.*' but not in code",
  gp = "only include existing functions and methods in manual 'usage' (via R CMD check)"
)

CHECKS$rcmdcheck_data_sets_in_usages_not_in_code <- make_rcmd_check(

  "Manual page 'usage' refers to data that exists",
  pattern = "Data with usage in documentation object '.*' but not in code",
  gp = "only include existing data in manual page 'usage' (via R CMD check)"
)

CHECKS$rcmdcheck_arguments_documented <- make_rcmd_check(

  "All function arguments are documented",
  pattern = "Argument names in code not in docs",
  gp = "document all arguments of all functions (via R CMD check)"
)

CHECKS$rcmdcheck_arguments_exist <- make_rcmd_check(

  "All documented function arguments exist",
  pattern = "Argument names in docs not in code",
  gp = "only include existing function arguments in the manual (via R CMD check)"
)

CHECKS$rcmdcheck_argument_names_match <- make_rcmd_check(

  "Argument names match in code and documentation",
  pattern = "Mismatches in argument names",
  gp = "make argument names in code and documentation match (via R CMD check)"
)

CHECKS$rcmdcheck_default_arguments_match <- make_rcmd_check(

  "Argument default values match in code and documentation",
  pattern = "Mismatches in argument default values",
  gp = "match default arguments in code and documentation (via R CMD check)"
)

## TODO: Codoc mismatches from documentation object
## https://github.com/wch/r-source/blob/a1e144e097996ab6cddcd833ad4bbd99e8398b0a/src/library/tools/R/QC.R#L877

CHECKS$rcmdcheck_codoc_s4_match <- make_rcmd_check(

  "S4 class code matches documentation",
  pattern  = "S4 class codoc mismatches from documentation object",
  gp = "keep documentation of S4 classes current (via R CMD check)"
)

CHECKS$rcmdcheck_codoc_data_match <- make_rcmd_check(

  "Data documentation and code match",
  pattern = "Data codoc mismatches from documentation object",
  gp = "keep data documentation current (via R CMD check)",
)

CHECKS$rcmdcheck_arguments_documented <- make_rcmd_check(

  "All arguments are documented",
  pattern = "Undocumented arguments in documentation object",
  gp = "document all function arguments (via R CMD check)"
)

CHECKS$rmcdcheck_no_double_arguments <- make_rcmd_check(

  "No double \\argument tags",
  pattern = "Duplicated \\argument entries in documentation object",
  gp = "document each argument exactly once (via R CMD check)"
)

CHECKS$rcmdcheck_arguments_in_usage <- make_rcmd_check(

  "All arguments are in 'usage'",
  pattern = "Documented arguments not in \\usage in documentation object",
  gp = "include all arguments in 'usage' (via R CMD check)"
)

CHECKS$rcmdcheck_usage_in_alias <- make_rcmd_check(

  "All objects in 'usage' have an 'alias'",
  pattern = "Objects in \\usage without \\alias in documentation object",
  gp = "include all objects from 'usage' in an 'alias' (via R CMD check)"
)

CHECKS$rcmdcheck_assignments_in_usage <- make_rcmd_check(

  "No assignments in 'usage'",
  pattern = "Assignments in \\usage in documentation object",
  gp = "avoid assignments in 'usage' in the documentation (via R CMD check)"
)

CHECKS$rcmdcheck_valid_usage <- make_rcmd_check(

  "'usage lines are valid in the manual",
  pattern = "Bad \\usage lines found in documentation object",
  gp = "have valid 'usage' lines in the manual (via R CMD check)"
)

CHECKS$rcmdcheck_S3_methods_without_full_name <- make_rcmd_check(

  "S3 methods have the correct syntax in the manual",
  pattern = "S3 methods shown with full name in documentation object",
  gp = "document S3 methods with the right syntax (via R CMD check)"
)

CHECKS$rcmdcheck_foreign_calls_have_package <- make_rcmd_check(

  "Foreign function calls have 'PACKAGE' argument",
  pattern = "Foreign function calls? without 'PACKAGE' argument",
  gp = "include the 'PACKAGE' argument in foreign language calls (via R CMD check)"
)

CHECKS$rcmdcheck_foreign_calls_non_empty_package <- make_rcmd_check(

  "'PACKAGE' argument in foreign calls is not empty",
  pattern = "Foreign function calls? with empty 'PACKAGE' argument",
  gp = "include a proper 'PACKAGE' argument in foreign calls (via R CMD check)"
)

CHECKS$rcmdcheck_foreign_calls_not_to_base <- make_rcmd_check(

  "Base package C/C++/Fortran code is not called",
  pattern = "Foreign function calls? to a base package",
  gp = "avoid calling C/C++/Fortran code in base packages (via R CMD check)"
)

CHECKS$rcmdcheck_foreign_calls_not_to_other_pkg <- make_rcmd_check(

  "C/C++/Fortran calls to same package only",
  pattern = "Foreign function calls? to a different package",
  gp = "avoid calling C/C++/Fortran code in other packages (via R CMD check)"
)

CHECKS$rcmdcheck_foreign_calls_to_declared_pkgs <- make_rcmd_check(

  "C/C++/Fortran calls to declared packaged only",
  pattern = "Undeclared packages? in foreign function calls",
  gp = "declaring packages called via foreign function calls (via R CMD check)"
)

CHECKS$rcmdcheck_register_foreign <- make_rcmd_check(

  "Register foreign function calls",
  pattern = "Registration problem",
  gp = "register foreign functions (via R CMD check)"
)

## TODO: Calls? with DUP
## https://github.com/wch/r-source/blob/a1e144e097996ab6cddcd833ad4bbd99e8398b0a/src/library/tools/R/QC.R#L2243

CHECKS$rcmdcheck_register_s3_methods <- make_rcmd_check(

  "Register S3 methods",
  pattern = "Found the following apparent S3 methods exported but not registered",
  gp = "register S3 methods (via R CMD check)"
)

CHECKS$rcmdcheck_avoid_t_and_f <- make_rcmd_check(

  "Use TRUE and FALSE instead of T and F",
  pattern = "found T/F in",
  gp = "use TRUE and FALSE instead of T and F (via R CMD check)"
)

CHECKS$rcmdcheck_circular_dependency <- make_rcmd_check(

  "Avoid circular package dependencies",
  pattern = "There is circular dependency in the installation order",
  gp = "avoid circular package dependencies (via R CMD check)"
)

CHECKS$rcmdcheck_required_pkgs_available <- make_rcmd_check(

  "Required packages are available",
  pattern = "Packages? required but not available",
  gp = "install all required packages before running this check (via R CMD check)"
)

CHECKS$rcmdcheck_suggested_pkgs_available <- make_rcmd_check(

  "Suggested packages are available",
  pattern = "Packages? suggested but not available",
  gp = "install all suggested package before running this check (via R CMD check)"
)

CHECKS$rcmdcheck_required_versions_are_ok <- make_rcmd_check(

  "Required packages have proper version",
  pattern = "Packages? required and available but unsuitable versions",
  gp = paste("install the proper versions of required packages before",
    "running this check (via R CMD check)")
)

## Former standard packages required but now defunct
## TODO

CHECKS$rcmdcheck_enhanced_pkgs_available <- make_rcmd_check(

  "Enhanced packages are available",
  pattern = "Packages? which this enhances but not available for checking",
  gp = "install all enhanced packages before running this check (via R CMD check)"
)

CHECKS$rcmdcheck_vignette_builder_declared <- make_rcmd_check(

  "Vignette builder is declared as a dependency",
  pattern = "VignetteBuilder packages? not declared",
  gp = "declare the vignette builder package as a dependency (via R CMD check)"
)

CHECKS$rcmdcheck_vignette_builder_available <- make_rcmd_check(

  "Vignette builder is available",
  pattern = "VignetteBuilder packages? required for checking but not installed",
  gp = "install the vignette builder package before running this check (via R CMD check)"
)

CHECKS$rcmdcheck_vignette_depends_declared <- make_rcmd_check(

  "Vignette dependencies (\\VignetteDepends) are declared",
  pattern = "Vignette dependencies \\(.* entries\\) must be contained in the DESCRIPTION Depends/Suggests/Imports entries",
  gp = "declare vignette dependencies (\\VignetteDepends entries) (via R CMD check)"
)

CHECKS$rcmdcheck_missing_namespace_depends <- make_rcmd_check(

  "Namespace dependencies are declared",
  pattern = "Namespace dependenc(ies|y) not required",
  gp = "declare packages you are importing from in DESCRIPTION (via R CMD check)"
)

CHECKS$rcmdcheck_too_many_depends <- make_rcmd_check(

  "Do not depend on many packages",
  pattern = "Adding so many packages to the search path",
  gp = "not 'Depend' on many packages (via R CMD check)"
)

CHECKS$rcmdcheck_linkingto_only <- make_rcmd_check(

  "Do not Depend/Import packages that only need LinkingTo",
  pattern = "Packages in Depends/Imports which should probably only be in LinkingTo",
  gp = paste("not Depend or Import packages that only need LinkingTo",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_valid_dep_pkg_name <- make_rcmd_check(

  "Package names in dependencies are valid",
  pattern = "Malformed package name",
  gp = paste("have a valid package name: starts with a letter,",
    "contains letters, numbers and dots and does not end with a dot",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_pkg_name_not_base <- make_rcmd_check(

  "Package name is different from base packages",
  pattern = "This is the name of a base package",
  gp = paste("choose a package name that does not coincide with",
    "base package names (via R CMD check)")
)

CHECKS$rcmdcheck_missing_encoding <- make_rcmd_check(

  "DESCRIPTION Encoding is OK",
  pattern = "Unknown encoding",
  gp = paste("declare encoding in DESCRIPTION, if it is not ASCII",
    "(via R CMD check)")
)

CHECKS$rmcdcheck_description_tags_ascii <- make_rcmd_check(

  "DESCRIPTION tags are ASCII",
  pattern = "All field tags must be ASCII",
  gp = "use only ASCII tags in DESCRIPTION (via R CMD check)"
)

CHECKS$rcmdcheck_good_version <- make_rcmd_check(

  "Version number must be valid",
  pattern = "Malformed package version",
  gp = paste("use a valid version number: two or three nonnegative integers,",
    "separated by a single dot or dash (via R CMD check)")
)

CHECKS$rcmdcheck_good_maintainer <- make_rcmd_check(

  "Maintainer field is valid",
  pattern = "Malformed maintainer field",
  gp = "have a valid maintainer field: 'name <email address>' (via R CMD check)"
)

CHECKS$rcmdcheck_valid_dep_fields <- make_rcmd_check(

  "Dependency fields are valid",
  pattern = "Malformed Depends or Suggests or Imports or Enhances field.",
  gp = "have valid dependency fields in DESCRIPTION (via R CMD check)"
)

CHECKS$rcmdcheck_valid_vignette_builder <- make_rcmd_check(

  "Vignette builder is valid",
  pattern = "Invalid VignetteBuilder field",
  gp = "have a valid VignetteBuilder tag in DESCRIPTION (via R CMD check)"
)

CHECKS$rcmdcheck_valid_priority <- make_rcmd_check(

  "Priority field is not given",
  pattern = "Invalid Priority field",
  gp = "omit the 'Priority' entry in DESCRIPTION (via R CMD check)"
)

CHECKS$rcmdcheck_valid_title <- make_rcmd_check(

  "Title field is valid",
  pattern = "Malformed Title field: should not end in a period",
  gp = "not end the Title entry in DESCRIPTION with a period (via R CMD check)"
)

CHECKS$rcmdcheck_valid_description <- make_rcmd_check(

  "Description field is valid",
  pattern = "Malformed Description field: should contain one or more complete sentences",
  gp = paste("include one or more complete sentences in the Description",
    "field in DESCRIPTION (via R CMD check)")
)

CHECKS$rcmdcheck_deps_in_single_fields <- make_rcmd_check(

  "Each dependent package is listed once in Depends/Imports/Suggests/Enhances",
  pattern = "Packages listed in more than one of Depends, Imports, Suggests, Enhances",
  gp = paste("list each dependent package in only one of",
    "Depends/Imports/Suggests/Enhances (via R CMD check)")
)

CHECKS$rcmdcheck_linkingto_needs_src <- make_rcmd_check(

  "There is an 'src' directory if 'LinkingTo' is used in DESCRIPTION",
  pattern = "'LinkingTo' field is unused: package has no 'src' directory",
  gp = "omit the 'LinkingTo' field if the package has no compiled code (via R CMD check)"
)

CHECKS$rcmdcheck_versioned_linkingto_needs_r_302 <- make_rcmd_check(

  "Need R 3.0.2 for versioned LinkingTo",
  pattern = "Versioned 'LinkingTo' values? for .* (is|are) only usable in R >= 3.0.2",
  gp = "require R version 3.0.2 for versioned LinkingTo fields (via R CMD check)"
)

CHECKS$rcmdcheck_linkingto_needs_include <- make_rcmd_check(

  "'LinkingTo' field needs 'include' directory",
  pattern = "'LinkingTo' for .* (is|are) unused as (it has|they have) no 'include' directory",
  gp = paste("omit 'LinkingTo' field if dependent package has no 'include'",
    "directory (via R CMD check)")
)

CHECKS$rcmdcheck_bad_authors <- make_rcmd_check(

  "Valid Authors@R field",
  pattern = "Malformed Authors@R field",
  gp = "have a valid 'Authors@R' field in DESCRIPTION (via R CMD check)"
)

CHECKS$rcmdcheck_authors_no_cre <- make_rcmd_check(

  "'Authors@R' needs to have a 'cre' role",
  pattern = "Cannot extract Author field from Authors@R field",
  gp = "include an author (role 'cre') in 'Authors@R' (via R CMD check)"
)

CHECKS$rcmdcheck_authors_no_aut <- make_rcmd_check(

  "'Authors@R' needs to have an 'aut' role",
  pattern = "Authors@R field gives no person with author role.",
  gp = "include a person with author ('aut') role in 'Authors@R' (via R CMD check)"
)

CHECKS$rcmdcheck_authors_roles <- make_rcmd_check(

  "'Authors@R' people have roles",
  pattern = "Authors@R field gives persons with no valid roles",
  gp = "define a role for each person in 'Authors@R' (via R CMD check)"
)

CHECKS$rcmdcheck_authors_maintainer <- make_rcmd_check(

  "'Authors@R' has valid maintainer",
  pattern = "Cannot extract Maintainer field from Authors@R field",
  gp = "include a valid maintainer in the Authors@R field (via R CMD check)"
)

CHECKS$rcmdcheck_authors_one_maintainer <- make_rcmd_check(

  "'Authors@R' has a single maintainer",
  pattern = "Authors@R field gives more than one person with maintainer role",
  gp = "have a single package maintainer in 'Authors@R' (via R CMD check)"
)

CHECKS$rcmdcheck_authors_valid_maintainer <- make_rcmd_check(

  "'Authors@R' has valid maintainer",
  pattern = "Authors@R field gives no person with maintainer role, valid email address and non-empty name",
  gp = "include a valid maintainer in the Authors@R field (via R CMD check)"
)

CHECKS$rcmdcheck_authors_portable_encoding <- make_rcmd_check(

  "Portable DESCRIPTION Encoding",
  pattern = "Encoding '.*' is not portable",
  gp = "use a portable encoding in DESCRIPTION: latin1, latin2 or UTF-8 (via R CMD Check)"
)

CHECKS$rcmdcheck_authors_defined_encoding <- make_rcmd_check(

  "Encoding of DESCRIPTION is defined",
  pattern = "Unknown encoding with non-ASCII data",
  gp = "define Encoding if you have a non-ASCII DESCRIPTION (via R CMD check)"
)

CHECKS$rcmdcheck_use_standard_license <- make_rcmd_check(

  "Use a standard license",
  pattern = "Non-standard license specification:",
  gp = "use a standard software license (via R CMD check)"
)

CHECKS$rcmdcheck_use_valid_license <- make_rcmd_check(

  "Use a valid license",
  pattern = "Deprecated license",
  gp = "update the license to a non-deprecated one (via R CMD check)"
)

CHECKS$rcmdcheck_has_license_file <- make_rcmd_check(

  "LICENSE file(s) exist if needs to",
  pattern = "Invalid license file pointers",
  gp = paste("add required LICENSE or other file(s) for the chosen LICENSE",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_only_permitted_license_restrictions <- make_rcmd_check(

  "License only has allowed restrictions",
  pattern = "License components with restrictions not permitted",
  gp = paste("not add extensions to licenses that do not allow extensions",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_template_license_components <- make_rcmd_check(

  "Template licenses have their components",
  pattern = "License components which are templates and need",
  gp = paste("add the extra 'LICENSE' file for licenses that need it",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_portable_compiler_flags <- make_rcmd_check(

  "Portable compiler flags in Makevars",
  pattern = "Non-portable flags in variable",
  gp = "use portable compiler flags in Makevars"
)

CHECKS$rcmdcheck_avoid_overriding_user_site_compiler_flags <- make_rcmd_check(

  "Avoid overriding user/site compiler flags",
  pattern = "Variables overriding user/site settings",
  gp = "not override user/site compiler flags in Makevars (via R CMD check)"
)

CHECKS$rcmdcheck_makevars_in_and_makevars <- make_rcmd_check(

  "Package does not have Makevars.in and Makevars",
  pattern = sprintf("Package has both %s and %s.",
    sQuote("src/Makevars.in"), sQuote("src/Makevars")),
  gp = "only have one of Makevars and Makevars.in (via R CMD check)"
)

CHECKS$rcmdcheck_undefined_globals <- make_rcmd_check(

  "No undefined global functions and variables",
  pattern = "Undefined global functions or variables",
  gp = "have no references to undefined global functions and variables (via R CMD check)"
)

CHECKS$rcmdcheck_obsolete_pkgs_in_xrefs <- make_rcmd_check(

  "No xref references to obsolete packages",
  pattern = "Obsolete packages? .* in Rd xrefs",
  gp = "have no xref references to obsolete packages"
)

CHECKS$rcmdcheck_referenced_pkgs_available <- make_rcmd_check(

  "All referenced packages are available",
  pattern = "Packages? unavailable to check Rd xrefs",
  gp = "install all referenced packages before running this check (via R CMD check)"
)

CHECKS$rcmdcheck_referenced_pkgs_known <- make_rcmd_check(

  "All referenced packages are known",
  pattern = "Unknown packages? .* in Rd xrefs",
  gp = "install all referenced packages before running this check (via R CMD check)"
)

CHECKS$rcmdcheck_xref_pkgs_available <- make_rcmd_check(

  "All linked packages are available",
  pattern = "Missing link or links in documentation object",
  gp = "make sure all links in the documentation are correct (via R CMD check)"
)

CHECKS$rcmdcheck_marked_latin1_strings <- make_rcmd_check(

  "No latin1 strings in data",
  type = "note",
  pattern = "Note: found .* marked Latin-1 string",
  gp = "avoid latin1 strings in data (via R CMD check)"
)

CHECKS$rcmdcheck_marked_utf8_strings <- make_rcmd_check(

  "No UTF-8 strings in data",
  type = "note",
  pattern = "Note: found .* marked UTF-8 string",
  gp = "avoid utf8 strings in data (via R CMD check)"
)

CHECKS$rcmdcheck_marked_bytes_strings <- make_rcmd_check(

  "No \"bytes\" strings in data",
  type = "note",
  pattern = "Note: found .* string marked as \"bytes\"",
  gp = "avoid strings marked as bytes in data (via R CMD check)"
)

CHECKS$rcmdcheck_non_ascii_strings <- make_rcmd_check(

  "Encoding of non-ascii strings is marked",
  pattern = "Warning: found non-ASCII string",
  gp = paste("marking the encoding of non-ascii strings in data",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_bzip2_xz_need_new_r <- make_rcmd_check(

  "R >= 2.10 is needed for bzip2 and xz compressed data",
  pattern = "Warning: package needs dependence on R .>= 2.10.",
  gp = paste("declare that R >= 2.10 is needed for bzip2 or xz compressed",
    "data (via R CMD check)")
)

CHECKS$rcmdcheck_data_compressed_efficiently <- make_rcmd_check(

  "Data files are compressed with maximum compression",
  pattern = "Warning: large data files? saved inefficiently",
  gp = paste("compress data files with maximum compression level",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_valid_file_names <- make_rcmd_check(

  "Use valid file names",
  pattern = "Subdirectory '.*' contains invalid file names",
  gp = "use only valid files names (via R CMD check)"
)

CHECKS$rcmdcheck_avoid_first_lib <- make_rcmd_check(

  "Avoid .First.lib",
  pattern = "NB: .First.lib is obsolete and will not be used in R >= 3.0.0",
  gp = "avoid using .First.lib (via R CMD check)"
)

CHECKS$rcmdcheck_bad_arg_names <- make_rcmd_check(

  "Correct argument names for startup functions",
  pattern = "has wrong argument list",
  gp = "use correct argument names for startup functions (via R CMD check)"
)

CHECKS$rcmdcheck_bad_calls_for_startup_functions <- make_rcmd_check(

  "Package startup functions do not change the search path",
  pattern = "Package startup functions should not change the search path",
  gp = paste("not change the search path in package startup functions",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_package_startup_messages <- make_rcmd_check(

  "Package startup messages are generated correctly",
  pattern = sprintf("Package startup functions should use %s to generate messages.",
    sQuote("packageStartupMessage")),
  gp = paste("use 'packageStartupMessage' for package startup messages",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_package_startup_safe_calls <- make_rcmd_check(

  "Package startup functions do not call 'installed.packages'",
  pattern = sprintf("Package startup functions should not call %s.",
    sQuote("installed.packages")),
  gp = paste("not call 'installed.packages' in package startup functions",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_last_lib_exported <- make_rcmd_check(

  ".Last.lib is exported",
  pattern = "NB: .Last.lib will not be used unless it is exported",
  gp = "export .Last.lib, otherwise it is not used (via R CMD check)"
)

CHECKS$rcmdcheck_detach_function_arguments <- make_rcmd_check(

  "Detach functions have correct arguments",
  pattern = sprintf("Package detach functions should have one argument with name starting with %s.",
    sQuote("lib")),
  gp = "have the correct arguments in detach functions (via R CMD check)"
)

CHECKS$rcmdcheck_detach_does_not_unload_shared_lib <- make_rcmd_check(

  "Package detach functions do not call 'library.dynam.unload'",
  pattern = sprintf("Package detach functions should not call %s.",
    sQuote("library.dynam.unload")),
  gp = paste("not call 'library.dynam.unload' in package detach functions",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_no_global_assignments <- make_rcmd_check(

  "No assignments to the global environment",
  pattern = "Found the following assignments to the global environment",
  gp = "not assign to the global environment (via R CMD check)"
)

CHECKS$rcmdcheck_no_attach <- make_rcmd_check(

  "No calls to attach",
  pattern = "Found the following calls to attach",
  gp = "not call attach (via R CMD check)"
)

CHECKS$rcmdcheck_no_data_into_global_environment <- make_rcmd_check(

  "Not loading data into global environment",
  pattern = "Found the following calls to data() loading into the global environment",
  gp = "not load data into the global environment (via R CMD check)"
)

CHECKS$rcmdcheck_colon_imports_are_declared <- make_rcmd_check(

  "'::' and ':::' imports are declared",
  pattern = "'::' or ':::' imports not declared from:",
  gp = "declare all packages used via '::' and ':::' (via R CMD check)"
)

CHECKS$rcmdcheck_library_require_packges_declare <- make_rcmd_check(

  "'library' and 'require' calls are declared",
  pattern = "'library' or 'require' calls not declared from:",
  gp = "declare all packages used via 'library' or 'require' (via R CMD check)"
)

CHECKS$rcmdcheck_loadnamespace_requirenamespace_declare <- make_rcmd_check(

  "'loadNamespace' and 'requireNamespace' calls are declared",
  pattern = "'loadNamespace' or 'requireNamespace' calls not declared from",
  gp = paste("declare all packages used via 'loadNamespace' or",
    "'requireNamespace' (via R CMD check)")
)

CHECKS$rcmdcheck_library_to_depended <- make_rcmd_check(

  "No 'library' or 'require' calls to packages already in 'Depends'",
  pattern = "'library' or 'require' calls to .* already attached by Depends:",
  gp = paste("not to use 'library' or 'require' for a package alreadt in",
    "Depends (via R CMD check)")
)

CHECKS$rcmdcheck_no_library_or_require <- make_rcmd_check(

  "No 'library' or 'require' calls",
  pattern = "Please use :: or requireNamespace.. instead",
  gp = "avoid using 'library' and 'require' in packages (via R CMD check)"
)

CHECKS$rcmdcheck_imported_is_used <- make_rcmd_check(

  "Imported namespaces are used",
  pattern = "Namespaces? in Imports field not imported from",
  gp = "import from packages declared in 'Imports' (via R CMD check)"
)

CHECKS$rcmdcheck_depends_not_imported <- make_rcmd_check(

  "Import from packages in 'Depends'",
  pattern = "Packages? in Depends field not imported from:",
  gp = "import from packages in 'Depends' field (via R CMD check)"
)

CHECKS$rcmdcheck_missing_or_unexported <- make_rcmd_check(

  "All imported functions are exported from the other package(s)",
  pattern = "Missing or unexported objects",
  gp = paste("only import functions that are exported from the other package",
    "(via R CMD check)")
)

CHECKS$rcmdcheck_triple_colon_instead_of_double <- make_rcmd_check(

  "Use '::' for exported functions, not ':::'",
  pattern = "':::' calls which should be '::':",
  gp = "use '::' for exported functions, not ':::' (via R CMD check)"
)

CHECKS$rcmdcheck_missing_triple_colon <- make_rcmd_check(

  "Objects in ':::' exist",
  pattern = "Missing objects imported by ':::' calls",
  gp = "make sure objects in ':::' calls exist (via R CMD check)"
)

CHECKS$rcmdcheck_triple_colon <- make_rcmd_check(

  "Avoid using ':::'",
  pattern = "Unexported objects imported by .*':::' calls?",
  gp = "avoid using ':::', only import exported functions (via R CMD check)"
)

CHECKS$rcmdcheck_triple_colon_to_self <- make_rcmd_check(

  "Avoid ':::' for self",
  pattern = "A package almost never needs to use ::: for its own objects:",
  gp = "avoid using ':::' for the package itself (via R CMD check)"
)

CHECKS$rcmdcheck_triple_colon_declared <- make_rcmd_check(

  "Declare packages used in ':::' calls",
  pattern = "Unavailable namespaces? imported from by .*':::' calls?",
  gp = "declare packages used in ':::' calls (via R CMD check)"
)

CHECKS$rcmdcheck_data_packages_declared <- make_rcmd_check(

  "Packages used for data are declared",
  pattern = "'data.package=.' calls? not declared from",
  gp = "declare packages used 'data(package=)' as dependencies (via R CMD check)"
)

CHECKS$rcmdcheck_global_t_or_f <- make_rcmd_check(

  "TRUE and FALSE is used instead of T and F",
  pattern = "Found possibly global 'T' or 'F' in the following function",
  gp = "avoid using 'T' and 'F' instead of 'TRUE' and 'FALSE' (via R CMD check)"
)

CHECKS$rcmdcheck_global_t_or_f_in_manual <- make_rcmd_check(

  "TRUE and FALSE is used instead of T and F, in the manual",
  pattern = "Found possibly global 'T' or 'F' in the examples of the following Rd file",
  gp = paste("avoid using 'T' and 'F' instead of 'TRUE' and 'FALSE'",
    "in manual examples (via R CMD check)")
)

CHECKS$rcmdcheck_no_internal_calls <- make_rcmd_check(

  "No calls to .Internal",
  pattern = "Found .*.Internal calls? in the following functions?:",
  gp = "avoid calling '.Internal' (via R CMD check)"
)

CHECKS$rcmdcheck_no_internal_calls_s4 <- make_rcmd_check(

  "No calls to .Internal in S4 methods",
  pattern = "Found .*.Internal calls? in methods for the following S4 generic",
  gp = "avoid calling '.Internal' (via R CMD check)"
)

CHECKS$rcmdcheck_no_internal_calls_rc <- make_rcmd_check(

  "No calls to .Internal in reference classes",
  pattern = "Found .*.Internal calls? in methods for the following reference class",
  gp = "avoid calling '.Internal' (via R CMD check)"
)

CHECKS$rcmdcheck_invalid_namespace <- make_rcmd_check(

  "NAMESPACE file is valid",
  type = "error",
  pattern = "Invalid NAMESPACE file, parsing gives",
  gp = "have a valid NAMESPACE file"
)
