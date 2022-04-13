#' Parses a file INI
#'
#' @name ini_parse
#' @export
#' @param filename filename to parse
#' @return a list containing the INI files' informations
#' @include utils.r


ini_parse <- function(filename) {
  stopifnot(configr::is.ini.file(file = filename))
  configr::read.config(filename)
}
