
get_package_name <- function(path = ".") {
  state <- parseNamespaceFile(basename(path), file.path(path, ".."))
}
