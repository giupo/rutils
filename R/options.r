#' Handling of options in R sucks chode big time.
#'
#' I'm gonna let parametrize the key for options
#'
#' @name setOption
#' @param key key for the option
#' @param value value for the option
#' @export

setOption <- function(key, value) {
  if(is.character(value)) {
    value <- shQuote(value, type="sh") # what a shameful hack
  }
  
  if(is.null(value)) {
    value <- "NULL"
  }
  eval(parse(text=paste0("options(",key, "=", value,")")))
}
