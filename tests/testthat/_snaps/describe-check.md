# describe_check

    Code
      describe_check(all_checks())
    Output
      $clean_userspace
      [1] "Examples and tests do not leave files behind"
      
      $covr
      [1] "All code is unit tested"
      
      $cyclocomp
      [1] "Functions are simple"
      
      $data_doc
      [1] "Data objects are documented"
      
      $no_description_depends
      [1] "No \"Depends\" in DESCRIPTION"
      
      $no_description_date
      [1] "No \"Date\" in DESCRIPTION"
      
      $description_url
      [1] "URL in DESCRIPTION"
      
      $description_bugreports
      [1] "BugReports in DESCRIPTION"
      
      $docs_return
      [1] "Exported functions should have a @return roxygen tag"
      
      $examples_dontrun
      [1] "Examples do not use \\dontrun"
      
      $examples_all_nonrunnable
      [1] "Examples have runnable code outside \\dontrun/\\donttest"
      
      $export_or_nord
      [1] "Functions have roxygen @export or @noRd tags"
      
      $nord_has_keywords_internal
      [1] "Functions tagged @noRd also have @keywords internal"
      
      $export_and_keywords_internal
      [1] "@export and @keywords internal should not co-exist"
      
      $exports_have_examples
      [1] "Exported functions have @examples or @example"
      
      $lintr_assignment_linter
      [1] "'<-' and not '=' is used for assignment"
      
      $lintr_line_length_linter
      [1] "Code lines are short"
      
      $lintr_semicolon_linter
      [1] "No trailing semicolons"
      
      $lintr_attach_detach_linter
      [1] "Avoid attach and detach"
      
      $lintr_setwd_linter
      [1] "Avoid setwd in R packages"
      
      $lintr_sapply_linter
      [1] "Avoid sapply"
      
      $lintr_library_require_linter
      [1] "Avoid library and require in packages"
      
      $lintr_seq_linter
      [1] "Avoid 1:length(...) and similar expressions"
      
      $no_import_package_as_a_whole
      [1] "Packages are not imported as a whole"
      
      $no_export_pattern
      [1] "exportPattern in NAMESPACE file"
      
      $rcmdcheck_package_directory_exists
      [1] "Package directory exists"
      
      $rcmdcheck_vignette_leftovers
      [1] "No vignette leftovers"
      
      $rcmdcheck_qpdf_present_for_checking
      [1] "'qpdf' must be installed for checks"
      
      $rcmdcheck_vignettes_at_right_place
      [1] "Vignettes are in 'vignettes'"
      
      $rcmdcheck_portable_file_names
      [1] "File names are portable"
      
      $rcmdcheck_duplicate_file_names
      [1] "No file names that only differ in case"
      
      $rcmdcheck_ascii_file_names
      [1] "File names are portable (ASCII)"
      
      $rcmdcheck_portable_paths
      [1] "Paths are short enough to be portable"
      
      $rcmdcheck_proper_permissions
      [1] "Files have correct permissions"
      
      $rcmdcheck_executable_files
      [1] "configure and cleanup files must be executable"
      
      $rcmdcheck_description_meta
      [1] "DESCRIPTION meta information is correct"
      
      $rcmdcheck_malformed_title_or_description
      [1] "Title and Description fields are valid"
      
      $rcmdcheck_portable_description_encoding
      [1] "Portable DESCRIPTION Encoding"
      
      $rcmdcheck_description_encoding_known
      [1] "Encoding of DESCRIPTION is defined"
      
      $rcmdcheck_description_ascii_tags
      [1] "All tags in DESCRIPTION are ASCII"
      
      $rcmdcheck_description_ascii_fields
      [1] "All tags in DESCRIPTION are ASCII, or Encoding is defined"
      
      $rcmdcheck_standardizable_license
      [1] "Package license is standardizable"
      
      $rcmdcheck_license_file_pointers
      [1] "License file pointers are correct"
      
      $rcmdcheck_non_standard_license
      [1] "Non-standard, but standardizable license"
      
      $rcmdcheck_deprecated_license
      [1] "Deprecated license"
      
      $rcmdcheck_not_permitted_license_restrictions
      [1] "Not permitted license restrictions"
      
      $rcmdcheck_template_licenses
      [1] "Template licenses have a LICENSE file"
      
      $rcmdcheck_authors_at_r_field
      [1] "Valid Authors@R field"
      
      $rcmdcheck_valid_author_in_authors_at_r
      [1] "Can extract Author from Authors@R"
      
      $rcmdcheck_author_in_authors_at_r
      [1] "Author is in Authors@R"
      
      $rcmdcheck_authors_at_r_valid_roles
      [1] "Authors@R has valid roles for all authors"
      
      $rcmdcheck_maintainer_in_authors_at_r
      [1] "Authors@R has package Maintainer"
      
      $rcmdcheck_single_maintainer_in_authors_at_r
      [1] "Authors@R has one package Maintainer only"
      
      $rcmdcheck_valid_maintainer_in_authors_at_t
      [1] "Authors@R has a valid Maintainer role person"
      
      $rcmdcheck_stale_author
      [1] "No stale Author field if Authors@R is present"
      
      $rcmdcheck_stale_maintainer
      [1] "No state Maintainer field if Authors@R is present"
      
      $rcmdcheck_needscompilation_field_valid
      [1] "NeedsCompilation field must be 'yes' or 'no'"
      
      $rcmdcheck_multiple_dependency_modes
      [1] "Packages should be listed as one type of dependency (except for LinkingTo)"
      
      $rcmdcheck_unused_linkingto
      [1] "LinkingTo field makes sense, there are headers to include"
      
      $rcmdcheck_versioned_linkingto
      [1] "Versioned 'LinkingTo' needs R >= 3.0.2"
      
      $rcmdcheck_linkingto_without_compiled_code
      [1] "'LinkingTo' field only makes sense if package has compiled code"
      
      $rcmdcheck_broken_vignettes_1
      [1] "Vignette output not in build/vignette.rds"
      
      $rcmdcheck_broken_vignettes_2
      [1] "All outputs from build/vignette.rds are in the package"
      
      $rcmdcheck_defunct_top_level
      [1] "Some top level files are defunct: install.R, R_PROFILE.R"
      
      $rcmdcheck_valid_readme_and_news
      [1] "README.md and NEWS.md must work with pandoc"
      
      $rcmdcheck_stale_license_file
      [1] "LICENSE (and LICENCE) must be mentioned in DESCRIPTION"
      
      $rcmdcheck_stale_inst_license_file
      [1] "inst/LICENSE (and inst/LICENCE) must be mentioned in DESCRIPTION"
      
      $rcmdcheck_non_standard_top_level_files
      [1] "Top-level files are standardized"
      
      $rcmdcheck_copyright_file_in_inst
      [1] "Copyright file should be in inst/"
      
      $rcmdcheck_authors_in_inst
      [1] "AUTHORS file should be in inst/"
      
      $rcmdcheck_leftover_files
      [1] "Check for left-over files"
      
      $rcmdcheck_empty_index_file
      [1] "INDEX file must not be empty if present"
      
      $rcmdcheck_empty_demo_index_file
      [1] "demo/00Index must not be empty if present"
      
      $rcmdcheck_proper_demo_index_file
      [1] "demo/00Index file is valid if it exists"
      
      $rcmdcheck_demos_are_indexed
      [1] "All demos are in demo/00Index"
      
      $rcmdcheck_stale_demos_in_index
      [1] "Demos in the index exist"
      
      $rcmdcheck_vignette_index
      [1] "Vignettes have index entries"
      
      $rcmdcheck_has_r_source_files
      [1] "Package must have R source files (or R/sysdata.rda) if R directory is present"
      
      $rcmdcheck_r_directory_uppercase
      [1] "R directory must be upper case"
      
      $rcmdcheck_man_directory_lowercase
      [1] "man directory must be lower case"
      
      $rcmdcheck_data_directory_lowercase
      [1] "data directory must be lower case"
      
      $rcmdcheck_rcheck_directories
      [1] "No leftover .Rcheck directories"
      
      $rcmdcheck_leftover_dvi_pdf_build_directories
      [1] "No leftover Rd2dvi or Rd2pdf directories"
      
      $rcmdcheck_leftover_vs_directories
      [1] "No leftover .git, .svn (etc.) version control directories"
      
      $rcmdcheck_invalid_file_names
      [1] "File names in some directories are restricted"
      
      $rcmdcheck_empty_data_directory
      [1] "data directory must not be empty, if it exists"
      
      $rcmdcheck_empty_demo_directory
      [1] "demo directory must not be empty, if it exists"
      
      $rcmdcheck_demos_are_ascii
      [1] "Demos should use ASCII characters"
      
      $rcmdcheck_demos_have_valid_code
      [1] "Demos should contain valid R code"
      
      $rcmdcheck_empty_exec_directory
      [1] "exec directory must not be empty, if it exists"
      
      $rcmdcheck_empty_inst_directory
      [1] "inst directory must not be empty, if it exists"
      
      $rcmdcheck_src_without_sources
      [1] "src directory must contain some source files, if it exists"
      
      $rcmdcheck_inst_interference
      [1] "Some directory names in inst are not allowed"
      
      $rcmdcheck_news_rd_is_valid
      [1] "inst/NEWS.Rd is valid if present"
      
      $rcmdcheck_citation_file_is_valid
      [1] "inst/CITATION is valid if present"
      
      $rcmdcheck_citation_file_at_standard_place
      [1] "CITATION is in inst/, not somewhere else"
      
      $rcmdcheck_r_files_are_ascii
      [1] "R files should be ASCII"
      
      $rcmdcheck_errors_in_r_files
      [1] "R files do not have syntax errors"
      
      $rcmdcheck_warnings_in_r_files
      [1] "R files do not throw warnings"
      
      $rcmdcheck_undeclared_imports
      [1] "'::' and ':::' imports must be declared"
      
      $rcmdcheck_undeclared_library_require
      [1] "'library' and 'require' calls must be declared"
      
      $rcmdcheck_undeclared_loadnamespace_requirenamespace
      [1] "'loadNamespace' and 'requireNamespace' calls must be declared"
      
      $rcmdcheck_library_require_to_attached
      [1] "'library' and 'require' calls to attached packages are not needed"
      
      $rcmdcheck_library_require_in_package_code
      [1] "Do not use 'library' or 'require' in package code. Use '::' or 'requireNamespace' instead"
      
      $rcmdcheck_imports_not_imported_from
      [1] "Packages declared in 'Imports' must be imported from"
      
      $rcmdcheck_depends_not_imported_from
      [1] "Packages in 'Depends' must be imported from"
      
      $rcmdcheck_missing_or_unexported_objects
      [1] "Exported objects must be present"
      
      $rcmdcheck_unneeded_triple_colon
      [1] "Use '::' to call exported functions, not ':::'"
      
      $rcmdcheck_triple_colon_imported_objects_exist
      [1] "All objects imported via ':::' must exist"
      
      $rcmdcheck_unexported_base_objects_imported
      [1] "':::' imports to unexported objects, from base packages"
      
      $rcmdcheck_unexported_objects_imported
      [1] "':::' imports to unexported objects"
      
      $rcmdcheck_triple_colon_to_itself
      [1] "':::' imports to the package itself are usually not needed"
      
      $rcmdcheck_triple_colon_to_unknown
      [1] "':::' imports to unavailable packages"
      
      $rcmdcheck_data_call_to_undeclared_package
      [1] "Packages used in 'data(package=)' must be declared"
      
      $rcmdcheck_exported_s3_methods_are_registered
      [1] "Exported S3 methods are registered"
      
      $rcmdcheck_replacement_function_arg_names
      [1] "Replacement functions must have a 'value' argument"
      
      $rcmdcheck_foreign_calls_have_package_argument
      [1] "Foreign calls must have a 'PACKAGE' argument"
      
      $rcmdcheck_foreign_call_empty_package_argument
      [1] "'PACKAGE' argument of foreign calls must not be empty"
      
      $rcmdcheck_foreign_call_to_base_package
      [1] "Foreign calls should not call base packages"
      
      $rcmdcheck_foreign_calls_to_another_package
      [1] "Should not call another package with foreign calls"
      
      $rcmdcheck_foreign_calls_to_unknown_package
      [1] "Packages called from foreign calls must be declared"
      
      $rcmdcheck_foreign_registration_problems
      [1] "Check for registration problems of foreign function calls"
      
      $rcmdcheck_foreign_calls_with_dup
      [1] "The 'DUP' argument of foreign calls is ignored"
      
      $rcmdcheck_loading_and_unloading
      [1] "Check loading and unloading the package"
      
      $rcmdcheck_first_lib_obsolete
      [1] ".First.lib is obsolete"
      
      $rcmdcheck_startup_function_arguments
      [1] "Package startup function arguments should be lib* and pkg*"
      
      $rcmdcheck_startup_function_change_search_path
      [1] "Package startup functions should not change the search path. I.e. do not call 'library' or 'require'"
      
      $rcmdcheck_startup_function_messages
      [1] "Package startup functions should use 'packageStartupMessage' to generate messages"
      
      $rcmdcheck_statup_function_unsafe_calls
      [1] "Package startup functions should not call installed.packages"
      
      $rcmdcheck_last_lib_needs_to_be_exported
      [1] ".Last.lib will not be used unless it is exported"
      
      $rcmdcheck_detach_function_arguments
      [1] "Package detach functions should have one argument with name starting with 'lib'"
      
      $rcmdcheck_detach_no_library_dynam_unload
      [1] "Package detach functions should not call 'library.dynam.unload'"
      
      $rcmdcheck_unsafe_calls
      [1] "Check for possibly unsafe calls: unlockBinding, assignInNamespace"
      
      $rcmdcheck_partial_argument_match
      [1] "Partial argument matches are fragile"
      
      $rcmdcheck_undefined_globals
      [1] "Check for undefined globals"
      
      $rcmdcheck_avoid_internal_calls
      [1] "No .Internal calls are allowed"
      
      $rcmdcheck_avoid_internal_calls_s4
      [1] "No .Internal calls are allowed in S4 methods"
      
      $rcmdcheck_avoid_internal_calls_rc
      [1] "No .Internal calls are allowed in reference class methods"
      
      $rcmdcheck_assignment_to_globalenv
      [1] "Assignment to .GlobalEnv is not allowed"
      
      $rcmdcheck_avoid_using_attach
      [1] "It is better to avoid attach()"
      
      $rcmdcheck_data_into_globalenv
      [1] "Avoid loading data into the global environment"
      
      $rcmdcheck_obsolete_platform_specific
      [1] "Avoid calling obsolete and platform specific functions"
      
      $rcmdcheck_obsolete_platform_specific_s4
      [1] "Avoid calling obsolete and platform specific functions from S4 methods"
      
      $rcmdcheck_obsolete_platform_specific_rc
      [1] "Avoid calling obsolete and platform specific functions from reference class methods"
      
      $rcmdcheck_deprecated_functions
      [1] "Avoid calling deprecated functions"
      
      $rcmdcheck_defunct_removed_functions
      [1] "Avoid calling defunct and removed functions"
      
      $rcmdcheck_avoid_platform_specific_devices
      [1] "Avoid using platform specific devices"
      
      $rcmdcheck_rd_empty_sections
      [1] "Check Rd files for empty sections"
      
      $rcmdcheck_rd_problems
      [1] "Check Rd files for problems"
      
      $rcmdcheck_rd_duplicated_name
      [1] "Check for Rd files with duplicated \\name"
      
      $rcmdcheck_rd_duplicated_alias
      [1] "Check for Rd files with duplicated \\alias"
      
      $rcmdcheck_rd_long_code_lines
      [1] "Check for long code lines in Rd files"
      
      $rcmdcheck_rd_cross_references
      [1] "Check Rd links"
      
      $rcmdcheck_missing_docs
      [1] "Check for undocumented exported objects"
      
      $rcmdcheck_code_docs_mismatch
      [1] "Check for code/documentation mismatches"
      
      $rcmdcheck_rd_usage
      [1] "Check Rd \\usage sections"
      
      $rcmdcheck_rd_s3_usage
      [1] "S3 methods should use the \\method markup, not their full name"
      
      $rcmdcheck_rd_contents
      [1] "Check Rd contents"
      
      $rcmdcheck_unstated_dependencies_in_examples
      [1] "Check for unstated dependencies in examples"
      
      $rcmdcheck_data_contents
      [1] "Check files in 'data' directory, if exists"
      
      $rcmdcheck_undeclared_non_ascii_characters_in_data
      [1] "Check for undeclared non-ASCII characters in data"
      
      $rcmdcheck_non_ascii_characters_in_data
      [1] "Check for non-ASCII characters in data"
      
      $rcmdcheck_uncompressed_data_files
      [1] "Check for data files that could be compressed better"
      
      $rcmdcheck_bzip2_xz_requirement
      [1] "bzip2 or xz compression needs more recent R version"
      
      $rcmdcheck_uncompressed_sysdata
      [1] "Check if sysdata.rda could be compressed better"
      
      $rcmdcheck_unneeded_doc_extra
      [1] "Check for unneeded extra style files for documentation"
      
      $rcmdcheck_doc_extra_licenses
      [1] "Check if extra doc styles have license that requires the distribution of its original sources"
      
      $rcmdcheck_doc_tex_leftovers
      [1] "Check for possible leftover files for docs"
      
      $rcmdcheck_extra_files_in_docs
      [1] "Check for extra files in docs"
      
      $rcmdcheck_extra_directories_in_docs
      [1] "Check for extra directories in docs"
      
      $rcmdcheck_vignettes_in_vignettes
      [1] "Vignettes must be vignettes/"
      
      $rcmdcheck_knitr_leftovers
      [1] "Check for possible knitr leftover files"
      
      $rcmdcheck_pdf_file_sizes
      [1] "Check if PDF files could be made signicantly smaller"
      
      $rcmdcheck_pdf_file_sizes_gs
      [1] "Check if PDF files could be made much smaller with GhostScript and qpdf"
      
      $rcmdcheck_source_line_endings
      [1] "Source files have LF (Unix) line endings"
      
      $rcmdcheck_makefile_line_endings
      [1] "Makefile(s) have LF (Unix) line endings"
      
      $rcmdcheck_makefile_with_final_lf
      [1] "Makefile(s) have a final LF (newline character)"
      
      $rcmdcheck_non_portable_makevars
      [1] "Check for non-portable Makevars flags"
      
      $rcmdcheck_makevars_overriding_user_site
      [1] "Check for Makevars flags overriding user/site settings"
      
      $rcmdcheck_makevars_and_makevars_in
      [1] "Check if package has both Makevars and Makevars.in"
      
      $rcmdcheck_gnu_make_required
      [1] "Check if GNU make is a system requirement"
      
      $rcmdcheck_gnu_makefile_extensions
      [1] "Check if GNU make is needed (and is undeclared)"
      
      $rcmdcheck_blas_and_lapack_flags
      [1] "Check if $(BLAS_LIBS) are used when $(LAPACK_LIBS) are"
      
      $rcmdcheck_missing_flibs
      [1] "Check if $(FLIBS) is required and missing"
      
      $rcmdcheck_unsafe_calls_in_compiled_code
      [1] "Check for unsafe calls in compiled code"
      
      $rcmdcheck_loading_package
      [1] "Check loading the package"
      
      $rcmdcheck_unloading_package
      [1] "Check unloading the package"
      
      $rcmdcheck_namespace_can_be_loaded
      [1] "Check if namespace can be loaded"
      
      $rcmdcheck_namespace_can_be_loaded_safely
      [1] "Check if namespace can be loaded safely"
      
      $rcmdcheck_namespace_can_be_unloaded
      [1] "Check if namespace can be unloaded"
      
      $rcmdcheck_loading_when_not_on_search_path
      [1] "Packages must be able to load when not on the search path"
      
      $rcmdcheck_s3_method_registration
      [1] "S3 methods are properly registered"
      
      $rcmdcheck_encoding_in_ascii_locale
      [1] "Check if we are checking a package with non-ascii encoding in an ASCII locale"
      
      $rcmdcheck_examples_run
      [1] "Examples must run"
      
      $rcmdcheck_examples_run_without_warnings
      [1] "Examples shoudl run without warnings"
      
      $rcmdcheck_two_many_cores_used
      [1] "Package should not use more than two cores during checks"
      
      $rcmdcheck_can_collect_examples
      [1] "Can create a single file with all example"
      
      $rcmdcheck_unstated_dependencies_in_tests
      [1] "All packages needed for tests are declared"
      
      $rcmdcheck_tests_pass
      [1] "Tests"
      
      $rcmdcheck_unstated_dependencies_in_vignettes
      [1] "All packages needed for vignettes are declared"
      
      $rcmdcheck_vignette_output_present
      [1] "Output from all vignettes are present"
      
      $rcmdcheck_encoding_defined_in_vignettes
      [1] "Non-ASCII vignettes must define their encoding"
      
      $rcmdcheck_doc_makefile_uppercase
      [1] "inst/doc/Makefile is uppercase"
      
      $rcmdcheck_calling_r_from_makefile
      [1] "R is called properly from Makefile"
      
      $rcmdcheck_makefile_line_endings_2
      [1] "Makefile line ending should be LF (Unix)"
      
      $rcmdcheck_calling_rscript_from_makefile
      [1] "Rscript is called properly from Makefile"
      
      $rcmdcheck_correct_vignette_encodings
      [1] "Vignettes use the encoding they define"
      
      $rcmdcheck_vignettes_load_dependencies
      [1] "Vignettes can load dependencies"
      
      $rcmdcheck_vignettes_run
      [1] "Vignettes run"
      
      $rcmdcheck_vignettes_build
      [1] "Vignettes build"
      
      $rcmdcheck_vignettes_build_2
      [1] "Vignettes build"
      
      $rcmdcheck_can_convert_rd_to_pdf
      [1] "Can convert Rd manual to PDF"
      
      $rcmdcheck_can_convert_rd_to_pdf_2
      [1] "Can convert Rd manual to PDF"
      
      $rcmdcheck_pdf_without_hyperref
      [1] "PDF manual builds without hyperref"
      
      $rcmdcheck_executable_files_in_packages
      [1] "Package should not contain executable files"
      
      $rcmdcheck_hidden_files_and_directories
      [1] "Package does not have hidden files and directories"
      
      $rcmdcheck_installs
      [1] "Package installs"
      
      $rcmdcheck_significant_compilation_warnings
      [1] "Check for significant warnings when compiling C/C++/Fortran code"
      
      $rcmdcheck_other_compilation_warnings
      [1] "Check for significant warnings when compiling C/C++/Fortran code"
      
      $rcmdcheck_reasonable_installed_size
      [1] "Package size is reasonable when installed"
      
      $rcmdcheck_description_required_fields
      [1] "DESCRIPTION has required fields"
      
      $rcmdcheck_package_name_portable
      [1] "Package name is portable"
      
      $rcmdcheck_description_right_case
      [1] "DESCRIPTION is all upper case"
      
      $rcmdcheck_not_package_bundle
      [1] "Package bundles are defunct"
      
      $rcmdcheck_cran_incoming_feasibility_1
      [1] "CRAN incoming feasibility"
      
      $rcmdcheck_cran_incoming_feasibility_2
      [1] "CRAN incoming feasibility"
      
      $rcmdcheck_cran_incoming_feasibility_3
      [1] "CRAN incoming feasibility"
      
      $rcmdcheck_valid_namespace
      [1] "Valid NAMESPACE file"
      
      $rcmdcheck_empty_importfrom_in_namespace
      [1] "Check for empty importFrom command in NAMESPACE"
      
      $rcmdcheck_too_many_s3_methods
      [1] "R < 3.0.2 had a limit of 500 registered S3 methods"
      
      $rcmdcheck_package_dependencies_present
      [1] "Package dependencies are present for the check"
      
      $rcmdcheck_if_source_package
      [1] "Only *source* packages can be checked"
      
      $rcmdcheck_object_files_in_source_package
      [1] "Source packages should not contain object files"
      
      $rcmdcheck_multi_arch_build_dir
      [1] "Check for leftover multi-arch build directory"
      
      $rcmdcheck_compilation_leftover_files
      [1] "Possible leftover files from a compilation"
      
      $rcmdcheck_object_files_in_source_package_2
      [1] "Source packages should not contain object files"
      
      $rcmdcheck_installed_version_included
      [1] "Check if an installed version is included by mistake"
      
      $rcmdcheck_some_code_in_docs
      [1] "*Some* form of documentation should contain some code to run "
      
      $truefalse_not_tf
      [1] "TRUE and FALSE is used, not T and F"
      

