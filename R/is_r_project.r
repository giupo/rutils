#' Checks if `path` contains a valid R project structure
#'
#' Checks if `path` contains:
#' - a DESCRIPTION file
#' - a NAMESPACE file
#' - an R directory
#'
#' @md
#' @export

is_r_project <- function(path = getwd()) {
  file.exists(file.path(path, "DESCRIPTION")) &&
    file.exists(file.path(path, "NAMESPACE")) &&
    dir.exists(file.path(path, "R"))
}