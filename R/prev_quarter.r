#' Return the current quarter
#'
#' @param date the date of the quarter
#' @return the numeric repr of quarter

quarter <- function(date = Sys.Date()) {
  as.numeric(substr(base::quarters(date), 2, 2))
}



#' this function returns the previous quarter compared on the current date
#'
#' @param date date to evaluate previous year
#'   quarter. (defaults to `Sys.Date()`)
#' @export

prev_quarter <- function(date = Sys.Date()) {
  year <- lubridate::year(date)
  period <- quarter(date = date)

  if (period == 1) {
    year <- year - 1
    period <- 4
  } else {
    period <- period - 1
  }

  c(year, period)
}
