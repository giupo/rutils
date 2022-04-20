#' Parses an INI file
#'
#' @export
#' @param filename filename to parse
#' @return a list containing the INI files' informations
#' @include utils.r


ini_parse <- function(filename) {
  stopifnot(configr::is.ini.file(file = filename))
  configr::read.config(filename)
}

#' Parses a TOML file
#'
#' @export
#' @param filename filename to parse
#' @return a list containing the TOML files' informations
#' @include utils.r

toml_parse <- function(filename) {
  stopifnot(configr::is.toml.file(file = filename))
  configr::read.config(filename)
}
