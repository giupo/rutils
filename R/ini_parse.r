# Horrible hack
globalVariables(c("V1", "V2"))

#' Parses a file INI
#'
#' @name ini_parse
#' @export
#' @param filename filename to parse
#' @return a list containing the INI files' informations
#' @include utils.r
#' @importFrom methods new
#' @importFrom utils read.table
#' @note full credit to \url{http://goo.gl/V0jwlz}

ini_parse <- function(filename) {

  if(!file.exists(filename)) {
    stop(filename, " doesn't exists")
  }

  connection <- file(filename)
  on.exit(close(connection))
  
  lines <- readLines(connection)
  lines <- chartr("[]", "==", lines)  # change section headers
  textcon <- textConnection(lines)

  on.exit({
    close(textcon)
    close(connection)
  })
  
  d <- read.table(textcon, as.is = TRUE, sep = "=", fill = TRUE)

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
