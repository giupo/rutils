#' Parses a file INI
#'
#' @name ini_parse
#' @export
#' @param filename
#' @return a list containing the INI files' informations
#' @note full credit to \url{http://goo.gl/V0jwlz}

ini_parse <- function(filename) {

  if(!file.exists(filename)) {
    stop(filename, " doesn't exists")
  }

  connection <- file(filename)

  Lines  <- tryCatch(
    suppressWarnings(readLines(connection)),
    error = function(cond) {
        stop(filename, ":", cond)
    })

  close(connection)

  Lines <- chartr("[]", "==", Lines)  # change section headers

  connection <- textConnection(Lines)
  d <- read.table(connection, as.is = TRUE, sep = "=", fill = TRUE)
  close(connection)

  L <- d$V1 == ""                    # location of section breaks
  d <- subset(transform(d, V3 = V2[which(L)[cumsum(L)]])[1:3],
                           V1 != "")
  if(nrow(d) > 0) {
    ToParse  <- paste("INI.list$", d$V3, "$",  d$V1, ' <- "',
                      d$V2, '"', sep="")
    INI.list <- list()
    eval(parse(text=ToParse))
  }
  INI.list
}
