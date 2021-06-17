# Horrible hack
## globalVariables(c("V1", "V2"))

#' Parses a file INI
#'
#' @name ini_parse
#' @export
#' @param filename filename to parse
#' @return a list containing the INI files' informations
#' @include utils.r
#' @note full credit to \url{http://goo.gl/V0jwlz}

ini_parse <- function(filename) {
  V1 <- V2 <- NULL # nolint
  if (!file.exists(filename)) {
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

  d <- utils::read.table(textcon, as.is = TRUE, sep = "=", fill = TRUE)

  location <- d$V1 == ""                    # location of section breaks
  d <- subset(transform(d, V3 = V2[which(location)[cumsum(location)]])[1:3],
                           V1 != "")
  if (nrow(d) > 0) {
    to_parse  <- paste("ini_list$", d$V3,
      "$",  d$V1, ' <- "', d$V2, '"', sep = "")
    ini_list <- list()
    eval(parse(text = to_parse))
  }
  ini_list
}
