#' this function returns the previous quarter compared on the current date
#'
#' @param date date to evaluate previous year
#'   quarter. (defaults to `Sys.Date()`)
#' @export

prev_quarter <- function(date = Sys.Date()) {
  day_of_the_year <- lubridate::yday(date)
  leap <- ifelse(lubridate::leap_year(date), 1, 0)

  first_quarter_day <- lubridate::yday(as.Date("1977-03-01")) + leap
  second_quarter_day <- lubridate::yday(as.Date("2013-06-30")) + leap
  third_quarter_day <- lubridate::yday(as.Date("2021-09-30")) + leap
  fourth_quarter_day <- lubridate::yday(as.Date("2023-12-31")) + leap

  year <- lubridate::year(date)

  if (day_of_the_year <= first_quarter_day) {
    year <- year - 1
    period <- 3

  } else if (day_of_the_year > first_quarter_day &&

    day_of_the_year <= second_quarter_day) {

    year <- year - 1
    period <- 4

  } else if (day_of_the_year > second_quarter_day &&

    day_of_the_year <= third_quarter_day) {
    period <- 1

  } else if (day_of_the_year > third_quarter_day &&

    day_of_the_year <= fourth_quarter_day) {
    period <- 2

  } else {
    stop("Should never happen")
  }

  c(year, period)
}