create_random_chars <- function(n, values = letters) {
  paste0(sample(values, n, TRUE), collapse = "")
}


#' Generates a unique file name in a directory `prefix`
#'
#' it's safe to call this in a multithreaded environment
#'
#' @md
#' @param n number of character per file
#' @param prefix directory where the file must be created
#' @param ext filename extension
#' @param values available values for random filenamem creation
#'    defaults to `letters`
#' @return the path where it's safe to create a unique filename
#' @export

gen_unique_filename <- function(
  n = 7,
  prefix = tempdir(),
  ext = NA,
  values = letters
) {
  dir.create(prefix, showWarnings = FALSE, recursive = TRUE)

  # lock file management
  lock_file <- file.path(prefix, ".gen_unique_filename.lock")
  lock <- filelock::lock(lock_file)
  
  on.exit({
    filelock::unlock(lock)
    file.remove(lock_file)
  })

  generate_file_name <- function(n, ext = NA) {
    random_name <- create_random_chars(n, values = values)

    filename <- if (is.na(ext)) random_name
    else glue::glue("{random_name}.{ext}")

    file.path(prefix, filename)
  }

  filename <- generate_file_name(n, ext = ext)
  while (file.exists(filename)) {
    filename <- generate_file_name(n, ext = ext)
  }

  return(filename)
}
