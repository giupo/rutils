#' this function returns the previous quarter compared on the current date
#'
#' @usage prev_quarter()
#' @param date date to evaluate previous year
#'   quarter. (defaults to `Sys.Date()`)
#' @export

prev_quarter <- function(date = Sys.Date()) {
  prev_year_qtr <- zoo::as.yearqtr(date) - 0.25
  numeric_yearqtr <- as.numeric(prev_year_qtr)
  year <- floor(numeric_yearqtr)
  period <- (numeric_yearqtr - year) * 4 + 1
  c(year, period)
}