#' Validates a DESCRIPTION because nobody thought how to do it.
#' 
#' dumbasses.
#' 
#' @param path path to the DESCRIPTION file
#' @export

validate_description <- function(path = "DESCRIPTION") {
  if (!file.exists(path)) {
    stop("File DESCRIPTION non trovato.")
  }

  # Campi secondo Writing R Extensions (R Core manual)
  valid_fields <- c(
    "Package", "Type", "Title", "Version", "Date", "Author", "Authors@R",
    "Maintainer", "Description", "License", "URL", "BugReports",
    "Depends", "Imports", "LinkingTo", "Suggests", "Enhances",
    "SystemRequirements", "Encoding", "LazyData", "ByteCompile",
    "RoxygenNote", "Config/Needs/website", "Config/testthat/edition",
	"Roxygen", "Collate"
  )

  dcf <- read.dcf(path)
  fields <- colnames(dcf)

  unknown_fields <- setdiff(fields, valid_fields)
  missing_fields <- setdiff(c("Package", "Version", "Title", "Description"), fields)

  if (length(unknown_fields)) {
    warning("⚠️  Campi sconosciuti nel DESCRIPTION:\n  - ",
            paste(unknown_fields, collapse = "\n  - "))
  }

  if (length(missing_fields)) {
    warning("⚠️  Campi obbligatori mancanti:\n  - ",
            paste(missing_fields, collapse = "\n  - "))
  }

  if (!length(unknown_fields) && !length(missing_fields)) {
    message("✅ DESCRIPTION valido.")
  }
}
