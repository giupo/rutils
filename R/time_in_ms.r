#' Return the Epochtime in milliseconds
#'
#' @return the epoch time in ms
#' @export

time_in_ms <- function() {
  nt <- nanotime::nanotime(Sys.time())
  ms <- bit64::as.integer64(nt) / 1e6
  bit64::as.integer64(round(ms))
}
